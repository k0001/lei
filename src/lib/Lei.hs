{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | /Lei/ allows you to organize MVC applications in which:
--
-- * A single MVC application can be split to many MVC components, each
--   consisting of a controller, possibly a model state, and zero or more views.
--
-- * The model state is kept globally.
--
-- * Controllers take a request, interact with the real world, and then change
--   the model state.
--
-- * Controllers handling the same or different request and or model state types
--   can be composed using 'mappend'.
--
-- * Different controllers can communicate with each other bidirectionally and
--   share the same monad stack.
--
-- * Views render the model state.
--
-- * Views can render things other than 'IO' actions.
--
-- * Views have their own state which is also kept globally and can also be
--   nested.
--
-- * Views render only if the model state has changed.
--
-- * Views have their own initialization and deinitialization routines.
--
-- * Views can be nested, and their rendering results can be composed in any
--   way their types allow it.
--
-- * The changes to the state of the application can be monitored.
--
-- This module is intended to be imported this way:
--
-- > import qualified Lei
--
-- There are a lot of free type variables seen in this documentation, but all of
-- them follow the same naming convention. Here is a reference to keep handy
-- while you familiarize yourself with Lei:
--
-- * @s0@: the top-level model state type.
--
-- * @s @: the model state type at the current 'Controller' or 'View' layer.
--
-- * @s'@: the model state type one layer below the current 'Controller' layer.
--
-- * @r @: the request type at the current 'Controller' or 'View' layer,
--   to be handled by a compatible 'Controller'.
--
-- * @r0@: the top-level request state, to be handled by the top-level
--   'Controller'.
--
-- * @v @: the 'View' state type at the current 'View' layer.
--
-- * @s'@: the model state type one layer below the current 'Controller' layer.
--
-- * @m @: the monad where the 'Controller's and the 'ViewInit's run.
--
-- Work with the type system, don't fight it. Lei is designed in such a way that
-- you can rely to a great extent on the types and avoid a wide range of common
-- mistakes, while still being able to /compose/ large applications.
module Lei
  ( -- * Running a Lei application
    runSimple
  , run

  -- * Controller
  , Controller
  , mkController
  , controlling
  , controlling0
  , hoistController

  , C
  , req
  , stop
  , nestController
  , nestController0

  -- * View
  , View
  , mkView
  , mkViewSimple

  -- ** Initialization
  , ViewInit
  , nestView

  -- ** Rendering
  , ViewRender

  , VR
  , render

  -- * Debug
  , Debug(..)
  , mapDebug
  ) where

import           Control.Applicative
import qualified Control.Exception as Ex
import           Control.Concurrent.MVar as MVar
import           Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as STM
import           Control.Lens
import           Control.Monad (void, unless, ap, (<=<))
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Control.Monad.Morph (MFunctor(hoist))
import           Control.Monad.Trans.Class (MonadTrans(lift))
import           Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import qualified Control.Monad.State as State
import           Data.Data (Data)
import           Data.Foldable (mapM_)
import           Data.Function (fix)
import qualified Data.IORef as IORef
import           Data.Monoid (Monoid(..))
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import qualified Pipes
import           Prelude hiding (mapM_)

--------------------------------------------------------------------------------

-- | A 'Controller', constructed with 'mkController', is where you deal with a
-- concrete request of type @r@ and affect the “local” model state @s@ while
-- having access to a monad stack @m@.
--
-- The @r0@ and @s0@ are the top-level request and model state types, and they
-- indicate that 'Controller' can be nested within another controller sharing
-- the same @r0@ and @s0@, of which @r@ and @s@ are smaller pieces.
--
-- A 'Controller r0 s0 r s m' runs whenever there is an @r@ in @r0@, and for as
-- long as @s@ can be found within @s0@. This last bit is quite important, it
-- means that, at any time, a larger 'Controller' nesting the former can
-- remove @s@ from @s0@, which will cause the nested 'Controller' to
-- /gracefully/ stop excecuting right away.
--
-- 'controlling' and 'controlling0' allow you to nest 'Controllers' so that they
-- can be composed afterwards with 'mappend'.
newtype Controller r0 s0 r s m = Controller { unController :: r -> C r0 s0 r s m () }

-- | @'mappend' a b@: First @a@ handles @r@, and then @b@ handles the same @r@.
instance Monad m => Monoid (Controller r0 s0 r s m) where
  mempty = Controller (\_ -> return ())
  mappend a b = Controller (\r -> unController a r >> unController b r)

-- | See the documentation for 'Controller'.
mkController :: (r -> C r0 s0 r s m ()) -> Controller r0 s0 r s m -- ^
mkController = Controller
{-# INLINABLE mkController #-}

-- | 'hoist' the monad underlying this 'Controller'.
hoistController
  :: Monad m
  => (forall a. m a -> n a) -- ^ Monad morphism
  -> Controller r0 s0 r s m
  -> Controller r0 s0 r s n
hoistController nat = \c -> Controller (\r -> hoist nat (unController c r))
{-# INLINABLE hoistController #-}

-- | Nest a @'Controller' r0 s0 s' r' m@ inside a larger
-- @'Controller' r0 s0 s r m@ ensuring that it executes when as for as
-- long as it is explained in the documentation for 'Controller'.
--
-- By /larger/ we mean that @r'@ is potentially contained in @r@, and that
-- @s'@ is potentially contained in @s@.
controlling
  :: Monad m
  => Prism' r r'
  -- ^ Matches the @r'@s that will be handled by the nested controller.
  -> (s -> Maybe s', s' -> s -> s)
  -- ^ Think of this as the getter and the setter for a 'Traversal''
  -- targeting 0 or 1 elements, not more.
  -> ((C r0 s0 r s m a -> C r0 s0 r' s' m a) -> Controller r0 s0 r' s' m)
  -- ^ Returns the 'Controller' that will  hand requests matched by the given
  --   'Prism''. The given /natural transformation/ “downgrades” 'C' actions
  --   happening at the current 'Controller' layer so that they can be used
  --   on the nested 'Controller'.
  -> Controller r0 s0 r s m -- ^
controlling prr' xss' kcer = Controller (\r ->
    mapM_ (nestController kcer xss' (review prr')) (preview prr' r))
{-# INLINABLE controlling #-}

-- | Like 'controlling', but for nesting a top-level 'Controller'.
controlling0
  :: Monad m
  => Prism' r r'
  -> (s -> Maybe s', s' -> s -> s)
  -> Controller r' s' r' s' m
  -> Controller r0 s0 r  s  m -- ^
controlling0 prr' xss' cer = Controller (\r ->
    mapM_ (nestController0 cer xss' (review prr')) (preview prr' r))
{-# INLINABLE controlling0 #-}

runController0
  :: Monad m
  => Controller r s r s m
  -> r
  -> s
  -> Pipes.Producer (Maybe r) m s
runController0 cer r =
   State.execStateT (runMaybeT (runC (unController cer r) id (Just . id) const))

--------------------------------------------------------------------------------

-- | A @'C' r0 s0 r s m a@ is where you can issue requests of type @r@ and
-- modify a model state of type @s@ while having access to a monad @m@, as
-- described in the documentation for 'Controller'.
--
-- A 'C' needs be converted into a 'Controller' using 'controller' before
-- it can be used as (or as part of) a top-level 'Controller'.
--
-- It is important to know that a 'C' (and thus, a 'Controller'), runs until it
-- finishes on its own or until the model state can't be found on the top-level
-- state anymore. This allows top-level 'Controller' to alter their state
-- without causing child controllers to fail. This is handled automatically in
-- the background for you.
--
-- Things that you can do in a @'C' r0 s0 r s m a@:
--
-- * Everything @m@ allows.
--
-- * Request of type @r@ can be delivered asyncrhonously for another
--   'Controller' (possibly this very same one) to eventually handle it.
--   You use 'req' to issue a request.
--
-- * Change the model state. Changes happen atomically when the 'Controller'
--   runs.
--
-- * Nest 'Controllers'. In a similar spirit to 'controlling' and
--   'controlling0', the tools 'nestController' and 'nestController0' allow
--   you to nest a 'Controller' inside a 'C' explicitely.
newtype C r0 s0 r s m a = C
  { unC :: (r -> r0)
        -> (s0 -> Maybe s) -- A getter.
        -> (s -> s0 -> s0) -- A setter. It is OK to leave @s0@ untouched if @s@
                           -- has no place in it.
        -> MaybeT (State.StateT s0 (Pipes.Producer (Maybe r0) m)) a
  } deriving (Functor)

runC
  :: Monad m
  => C r0 s0 r s m a
  -> (r -> r0)
  -> (s0 -> Maybe s)
  -> (s -> s0 -> s0)
  -> MaybeT (State.StateT s0 (Pipes.Producer (Maybe r0) m)) a
runC c r2r0 gs0s ss0s = do
  s0 <- State.get
  case gs0s s0 of
     Nothing -> MaybeT (return Nothing)
     Just _  -> unC c r2r0 gs0s ss0s
{-# INLINE runC #-}

instance Monad m => Applicative (C r0 s0 r s m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (C r0 s0 r s m) where
  return a = C (\_ _ _ -> return a)
  ma >>= k = C (\r2r0 gs0s ss0s -> do
      a <- runC ma r2r0 gs0s ss0s
      runC (k a) r2r0 gs0s ss0s)

instance MonadIO m => MonadIO (C r0 s0 r s m) where
  liftIO = lift . liftIO

-- | Changes to @s@ won't be fully commited to the state until the
-- 'Controller' that embeds this 'C' finishes executing. Nevertheless,
-- within @C@, this 'State.MonadState' will behave as expected and will
-- always reflect the most recently desired state.
instance Monad m => State.MonadState s (C r0 s0 r s m) where
  state k = C (\_ gs0s ss0s -> MaybeT (State.state (\s0 ->
    case gs0s s0 of
       Nothing -> (Nothing, s0)
       Just s  -> let !(a, !s') = k s in (Just a, ss0s s' s0))))

instance MonadTrans (C r0 s0 r s) where
  lift = \ma -> C (\_ _ _ -> lift (lift (lift ma)))
  {-# INLINABLE lift #-}

instance MFunctor (C r0 s0 r s) where
  hoist nat = \(C f) -> C (fmap (fmap (fmap (hoist (hoist (hoist nat))))) f)
  {-# INLINABLE hoist #-}

-- | Issue a local request for a 'Controller' to eventually handle it.
req :: Monad m => r -> C r0 s0 r s m ()
req = \r -> C (\r2r0 _ _ -> lift (lift (Pipes.yield (Just (r2r0 r)))))
{-# INLINABLE req #-}

-- | Gracefully stop execution of the entire Lei application.
stop :: Monad m => C r0 s0 r s m ()
stop = C (\_ _ _ -> lift (lift (Pipes.yield Nothing)))
{-# INLINABLE stop #-}

-- | Nest a 'Controller' inside a 'C', in a similar spirit to 'controlling'.
nestController
  :: Monad m
  => ((C r0 s0 r s m a -> C r0 s0 r' s' m a) -> Controller r0 s0 r' s' m)
  -- ^ Returns the 'Controller' that will  hand requests matched by the given
  --   'Prism''. The given /natural transformation/ “downgrades” 'C' actions
  --   happening at the current 'Controller' layer so that they can be used
  --   on the nested 'Controller'.
  -> (s -> Maybe s', s' -> s -> s)
  -- ^ A getter and a setter. It is OK for this setter to leave the given
  -- @s@ untouched if the @s'@  has no place in it. TODO: Ideally I want to take
  -- a Traversal here, and for each target, run the nested controller. However,
  -- to do that, it seems I need to be able to convert a Traversal targeting N
 -- elements to N Traversals targeting 1 element.
  -- See some hints at https://github.com/ekmett/lens/issues/252
  -> (r' -> r)
  -> r'
  -> C r0 s0 r s m ()
nestController kcer (gss', sss') r'2r = \r' -> C (\r2r0 gs0s ss0s -> do
    let cer = kcer (\c' -> C (\_ _ _ -> runC c' r2r0 gs0s ss0s))
    runC (unController cer r')
         (r2r0 . r'2r)
         (gss' <=< gs0s)
         (\s' s0 -> case gs0s s0 of
             Nothing -> s0
             Just s  -> ss0s (sss' s' s) s0))

-- | Like 'nestController', but for nesting a top-level 'Controller'.
nestController0
  :: (Monad m)
  => Controller r' s' r' s' m
  -> (s -> Maybe s', s' -> s -> s)
  -> (r' -> r)
  -> r'
  -> C r0 s0 r s m () -- ^
nestController0 cer (gss', sss') r'2r = \r' ->
    C (\r2r0 gs0s ss0s -> MaybeT (State.StateT (\s0 -> do
       let ms' = gss' =<< gs0s s0
           ss0s' = \s' -> case gs0s s0 of
              Nothing -> s0
              Just s  -> ss0s (sss' s' s) s0
       case ms' of
          Nothing -> return (Nothing, s0)
          Just s' -> do
             s'_ <- Pipes.for (runController0 cer r' s')
                              (Pipes.yield . fmap (r2r0 . r'2r))
             return (Just (), ss0s' s'_))))
{-# INLINABLE nestController0 #-}

--------------------------------------------------------------------------------
-- | While having access to a view state @v@, render an @x@.
newtype VR v x = VR { unVR :: v -> IO (x, v) }
  deriving (Functor)

instance Monad (VR v) where
  return = \a -> VR (\v -> return (a, v))
  {-# INLINABLE return #-}
  (>>=) = \ma kmb -> VR (\v -> do
      (a, v') <- unVR ma v
      unVR (kmb a) v')
  {-# INLINABLE (>>=) #-}

instance Applicative (VR v) where
  pure = return
  {-# INLINABLE pure #-}
  (<*>) = ap
  {-# INLINABLE (<*>) #-}

-- | Warning: The 'IO' actions taking in place in 'VR' are intended to prepare
-- the rendering result, but they are not part of the rendering result itself
-- (i.e., the @x@ in @VR v r x@), which means they may not be executed each
-- time the rendering result is needed. If you intend some action to be executed
-- at the time of rendering, then return that action as the result of the @VR@
-- action, that is, have @'VR' v r ('IO' ())@ or similar.
instance MonadIO (VR v) where
  liftIO = \m -> VR (\v -> fmap (flip (,) v) m)
  {-# INLINABLE liftIO #-}

-- | View state.
instance State.MonadState v (VR v) where
  state k = VR (return . k)
  {-# INLINABLE state #-}

-- | Render a view that had been previously nested using 'nestView'.
--
-- @x@ is guaranteed to be sucessfully looked up from a cache if @s@ and @v@
-- haven't changed since the last time the given @'ViewRender' v s x@ was
-- 'render'ed.
render :: ViewRender v s x -> s -> VR v x
render vr = \s -> VR (unVR (unViewRender vr s))
{-# INLINABLE render #-}

--------------------------------------------------------------------------------

-- | Renders a model of type @s@ as an @x@. While rendering, access to the view
-- state @v@ is available for read and write purposes.
newtype ViewRender v s x = ViewRender { unViewRender :: s -> VR v x }
  deriving (Functor)

instance Monoid x => Monoid (ViewRender v s x) where
  mempty = ViewRender (\_ -> return mempty)
  {-# INLINE mempty #-}
  mappend a b =
      ViewRender (\s -> mappend <$> unViewRender a s <*> unViewRender b s)
  {-# INLINE mappend #-}

mkViewRenderCacheLast
  :: forall v s x
   . (Eq v, Eq s)
  => (s -> VR v x)
  -> IO (ViewRender v s x)
mkViewRenderCacheLast kvr = do
  iorCache <- IORef.newIORef ((\_ _ -> Nothing) :: s -> v -> Maybe (x, v))
  return (ViewRender (\s0 -> VR (\v0 -> do
     cacheLookup <- IORef.readIORef iorCache
     case cacheLookup s0 v0 of
        Just xv -> return xv
        Nothing -> do
           !xv@(!_,!_) <- unVR (kvr s0) v0
           IORef.atomicWriteIORef iorCache (\s v ->
              if s == s0 && v == v0 then Just xv else Nothing)
           return xv)))

--------------------------------------------------------------------------------
-- See the documentation for 'mkView'.
newtype View v r s m x
  = View { unView :: (r -> IO ())
                  -> IO ()
                  -> ViewInit v r m (v, ViewStop v, ViewRender v s x) }
  deriving (Functor)

instance MFunctor (View v r s) where
  hoist nat = \m -> View (\reqIO stopIO -> hoist nat (unView m reqIO stopIO))
  {-# INLINABLE hoist #-}

runView
  :: Monad m
  => View v r s m x
  -> (r -> IO ())
  -> IO ()
  -> m (v, ViewStop v, ViewRender v s x)
runView a reqIO stopIO = do
   ((v0, vs0, vrr0), vs) <- runViewInit (unView a reqIO stopIO) reqIO stopIO
   return (v0, mappend vs vs0, vrr0)
{-# INLINABLE runView #-}

-- | Creates a 'View' describing the lifetime of a 'ViewRender', consisting of
-- an initialization routine ('ViewInit') to be executed only once at the
-- beginning of the execution of the Lei application, which will return:
--
-- 1. An initial “view state” of type @v@. This will be available for
-- modification each time 'VR' runs.
--
-- 2. A deinitialization routine, which will called in case of execptions or
-- graceful shutdown of the Lei application including this 'View'. The most
-- recent value of the view state @v@ will be passed in.
--
-- 3. An action that given an @s@ will render an @x@ potentially issuing
-- requests of type @r@ and modifying the view state @v@. This action will
-- be executed only if at least one of @s@ or @v@ have changed since the last
-- time this action was executed.
--
-- The resulting 'View' can either be as a top-level view for a Lei application
-- using 'run', or it can be nested in larger 'View's using 'nestView'.
mkView
  :: (MonadIO m, Eq v, Eq s)
  => ((r -> IO ()) -> IO () -> ViewInit v r m (v, v -> IO (), s -> VR v x))
  -- ^ @r -> 'IO' ()@: issue a request. Note that, even if it is perfectly safe
  --   to do so, issuing requests from within the 'ViewInit' is not very useful.
  --   It's more likely that you will just want to pass this function to other
  --   tools setup during the view initialization process, such as nested views
  --   or third-party event handlers.
  --
  --   @'IO' ()@: gracefully stop the entire Lei application.
  --
  --   @v@: view state.
  --
  --   @v -> 'IO' ()@: release resources acquired by 'ViewInit'.
  --
  --   @s@: model state to render.
  --
  --   @x@: rendering result.
  -> View v r s m x
mkView kvi = View (\reqIO stopIO -> do
  (v, iovs, kvr) <- kvi reqIO stopIO
  vr <- liftIO (mkViewRenderCacheLast kvr)
  return (v, mkViewStop iovs, vr))
{-# INLINABLE mkView #-}

-- | Like 'mkView', except for when there is no view state, initialization nor
-- finalization to worry about.
mkViewSimple
  :: (Eq s, MonadIO m)
  => ((r -> IO ()) -> s -> x) -- ^ @s@: state to render.
                              --
                              --   @r -> 'IO' ()@: issue a request.
                              --
                              --   @x@: rendering result.
  -> View () r s m x
mkViewSimple kvr = mkView (\reqIO _ -> return ((), return, return . kvr reqIO))
{-# INLINABLE mkViewSimple #-}

--------------------------------------------------------------------------------

-- | Action to be executed just once during the initialization of a 'View',
-- preparing an initial view state of type @v@.
newtype ViewInit v r m a
  = ViewInit { unViewInit :: (r -> IO ()) -> IO () -> (State.StateT (ViewStop v) m a) }
  deriving (Functor)

instance Monad m => Monad (ViewInit v r m) where
  return = \a -> ViewInit (\_ _ -> return a)
  {-# INLINABLE return #-}
  ma >>= k = ViewInit (\reqIO stopIO -> do
     a <- unViewInit ma reqIO stopIO
     unViewInit (k a) reqIO stopIO)
  {-# INLINABLE (>>=) #-}

instance (Monad m, Functor m) => Applicative (ViewInit v r m) where
  pure = return
  {-# INLINABLE pure #-}
  (<*>) = ap
  {-# INLINABLE (<*>) #-}

instance MonadIO m => MonadIO (ViewInit v r m) where
  liftIO = lift . liftIO
  {-# INLINABLE liftIO #-}

instance MonadTrans (ViewInit v r) where
  lift = \m -> ViewInit (\_ _ -> lift m)
  {-# INLINABLE lift #-}

instance MFunctor (ViewInit v r) where
  hoist nat = \m -> ViewInit (\reqIO stopIO ->
      hoist nat (unViewInit m reqIO stopIO))
  {-# INLINABLE hoist #-}

runViewInit :: ViewInit v r m a -> (r -> IO ()) -> IO () -> m (a, ViewStop v)
runViewInit (ViewInit k) reqIO stopIO = State.runStateT (k reqIO stopIO) mempty

-- | Nest a smaller 'View' within a larger 'View'.
--
-- By “smaller” we mean that @v'@ is smaller than @v@, and that @r'@ is smaller
-- than @r@.
--
-- The resulting @v'@ is to be included in @v@, otherwise rendering the view
-- will fail. TODO: automate this step so that it can't be forgotten.
--
-- The resulting @'ViewRender' v s x@ is to be rendered insider using
-- 'render'.
nestView
 :: Monad m
 => Lens' v v'
 -> (r' -> r)
 -> View v' r' s m x
 -> ViewInit v r m (v', ViewRender v s x) -- ^
nestView lvv' r'2r av'w = ViewInit (\reqIO stopIO -> do
   (av', av's, av'rr) <- lift (runView av'w (reqIO . r'2r) stopIO)
   State.modify (mappend (contramapViewStop (view lvv') av's))
   let vrr = ViewRender (\s0 -> VR (\v -> do
          let v'1 = view lvv' v
          (x, v'2) <- unVR (unViewRender av'rr s0) v'1
          return (x, set lvv' v'2 v)))
   return (av', vrr))

--------------------------------------------------------------------------------


-- | Like 'run', but assumes that the application runs in 'IO' and doesn't
-- provide any monitoring capabilities.
runSimple
  :: Eq s
  => s
  -- ^ Initial /Model/ state.
  -> Controller r s r s IO
  -- ^ /Controller/ issuing controller requests and model operations.
  -> View v r s IO (IO ())
  -- ^ /View/ issuing controller requests and rendering the model.
  -> IO ()
runSimple = run Ex.bracket (\_ -> return ())

-- | Run a Lei application.
run
  :: forall r v s m a
   . (MonadIO m, Eq s)
  => (forall x y z . m x -> (x -> m y) -> (x -> m z) -> m z)
  -- ^ 'Control.Exception.bracket'-like function.
  --    Hint: use 'Control.Monad.Catch.bracket' from "Control.Monad.Catch".
  -> (IO (Debug v r s) -> IO a)
  -- ^ Loop monitoring events.
  -> s
  -- ^ Initial /Model/ state.
  -> Controller r s r s m
  -- ^ /Controller/ issuing controller requests and model operations.
  -- See 'controller'.
  -> View v r s m (IO ())
     -- ^ /View/ issuing controller requests and rendering the model.
  -> m ()
run bracket dbg0 s0 cer vw = do
    -- Debug broadcast channel
    tcbDebug <- liftIO STM.newBroadcastTChanIO
    -- Channel through which requests are sent
    tqR <- liftIO STM.newTQueueIO
    let reqIO = STM.atomically . STM.writeTQueue tqR
    -- TMVar where the last known model state is stored.
    -- 'Right' means it needs rendering.
    tmvSLast <- liftIO (STM.newTMVarIO (Right s0))
    -- TVar indicating whether to stop execution
    tvStop <- liftIO (STM.newTVarIO False) -- TODO: a `MVar ()` should do
    let stopIO = STM.atomically (do
          STM.writeTVar tvStop True
          STM.writeTChan tcbDebug DebugStopRequested)
    -- Handle an incomming requests
    let handlerLoop :: m ()
        handlerLoop = ($ s0) (fix (\loop s -> do
           stopped <- liftIO (STM.atomically (STM.readTVar tvStop))
           unless stopped (do
              r <- liftIO (STM.atomically (do
                 r <- STM.readTQueue tqR
                 r <$ STM.writeTChan tcbDebug (DebugReqStart r s)))
              !s' <- Pipes.runEffect (do
                 Pipes.for (runController0 cer r s) (\mr' -> do
                    case mr' of
                       Just r' -> liftIO (reqIO r')
                       Nothing -> liftIO stopIO >> lift (loop s)))
              liftIO (STM.atomically (do
                 void (STM.tryTakeTMVar tmvSLast)
                 if s == s'
                    then do STM.putTMVar tmvSLast (Left s')
                            STM.writeTChan tcbDebug (DebugStateSame s')
                    else do STM.putTMVar tmvSLast (Right s')
                            STM.writeTChan tcbDebug (DebugStateNew s')))
              loop s')))
    -- Render the model
    let renderLoop :: ViewStop v -> ViewRender v s (IO ()) -> v -> IO ()
        renderLoop vs vrr = fix (\loop v -> do
           flip Ex.onException (runViewStop vs v) (do
              stopped <- STM.atomically (STM.readTVar tvStop)
              if stopped
                 then runViewStop vs v
                 else do
                    Ex.bracketOnError
                       (STM.atomically (do
                           es <- STM.takeTMVar tmvSLast
                           case es of
                              Left _ -> STM.retry
                              Right s -> do
                                STM.writeTChan tcbDebug (DebugRenderStart v s)
                                return s))
                       (\s -> STM.atomically (do
                           void (STM.tryPutTMVar tmvSLast (Right s))))
                       (\s -> do
                           (io, v') <- unVR (unViewRender vrr s) v
                           io >> loop v')))
        debugging :: forall x. m x -> m x
        debugging k = bracket
           (liftIO (do
              tcbDebug' <- STM.atomically (STM.dupTChan tcbDebug)
              Async.async (dbg0 (STM.atomically (STM.readTChan tcbDebug')))))
           (liftIO . Async.cancel)
           (\_ -> k)

    debugging (bracket
       (do (v, vs, vrr) <- runView vw reqIO stopIO
           liftIO (do
              STM.atomically (STM.writeTChan tcbDebug (DebugViewInitialized v))
              flip Ex.onException (runViewStop vs v) (do
                 a1 <- Async.async (renderLoop vs vrr v)
                 a1 <$ Async.link a1)))
       (liftIO . Async.cancel)
       (\a1 -> handlerLoop >> liftIO (Async.wait a1)))

--------------------------------------------------------------------------------

data Debug v r s
  = DebugStopRequested
  | DebugViewInitialized v
  | DebugRenderStart v s
  | DebugReqStart r s
  | DebugStateNew s
  | DebugStateSame s
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

mapDebug
  :: (v -> v')
  -> (r -> r')
  -> (s -> s')
  -> Debug v r s
  -> Debug v' r' s' -- ^
mapDebug fv fr fs = \x -> case x of
    DebugStopRequested     -> DebugStopRequested
    DebugViewInitialized v -> DebugViewInitialized (fv v)
    DebugRenderStart v s   -> DebugRenderStart (fv v) (fs s)
    DebugReqStart r s      -> DebugReqStart (fr r) (fs s)
    DebugStateNew s        -> DebugStateNew (fs s)
    DebugStateSame s       -> DebugStateSame (fs s)

--------------------------------------------------------------------------------
-- Internal tools

-- | A 'v -> IO ()' that can be ran just once. The next times you try to run it,
-- it just returns @()@ without doing any actual work. It works across different
-- threads and in case of exceptions.
newtype ViewStop v = ViewStop (IO (MVar.MVar (Maybe (v -> IO ()))))

mkViewStop :: (v -> IO ()) -> ViewStop v
mkViewStop = ViewStop . MVar.newMVar . Just

runViewStop :: MonadIO m => ViewStop v -> v -> m ()
runViewStop (ViewStop iomv) v = liftIO $ do
    mv <- iomv
    Ex.bracket (MVar.takeMVar mv)
               (\_ -> MVar.putMVar mv Nothing)
               (mapM_ ($ v))

-- | @'mappend' a b@ runs @a@ first and then @b@, even in case of exceptions.
instance Monoid (ViewStop v) where
  mempty = mkViewStop $ const $ return ()
  mappend a b = mkViewStop $ \v -> runViewStop a v `Ex.finally` runViewStop b v

contramapViewStop :: (a -> b) -> ViewStop b -> ViewStop a
contramapViewStop f x = mkViewStop $ runViewStop x . f
