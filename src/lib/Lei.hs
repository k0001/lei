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
-- * @s @: the model state type at the current 'Controller' or 'View' layer.
--
-- * @s0@: the top-level model state type.
--
-- * @r @: the request type at the current 'Controller' or 'View' layer,
--   to be handled by a compatible 'Controller'.
--
-- * @r0@: the top-level request state, to be handled by the top-level
--   'Controller'.
--
-- * @v @: the 'View' state type at the current 'View' layer.
--
-- * @m @: the monad where the 'Controller's and the 'ViewInit's run.
module Lei
  ( -- * Running a Lei application
    runSimple
  , run

  -- * Controller
  , Controller
  , mkController
  , controlling

  , C
  , req
  , stop
  , bury
  , bury2
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
  , getStop
  , render

  -- * Debug
  , Debug(..)
  ) where

import           Control.Applicative
import qualified Control.Exception as Ex
import           Control.Concurrent.MVar as MVar
import           Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as STM
import           Control.Lens
import           Control.Monad (void, unless, ap, (<=<))
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Control.Monad.Trans.Class (MonadTrans(lift))
import           Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import qualified Control.Monad.State as State
import           Data.Data (Data)
import           Data.Foldable (traverse_)
import           Data.Function (fix)
import qualified Data.IORef as IORef
import           Data.Monoid (Monoid(..))
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import qualified Pipes
import           Pipes.Core ((//>))
import           Prelude hiding (sequence_)


--------------------------------------------------------------------------------

-- | A 'Controller', constructed with 'controller', is where you deal with a
-- concrete request of type @r@ using a given @'C' r0 o0 r o m a@.
newtype Controller r0 s0 r s m = Controller { unController :: r -> C r0 s0 r s m () }

-- | @'mappend' a b@: First @a@ handles @r@, and then @b@ handles the same @r@.
instance Monad m => Monoid (Controller r0 s0 r s m) where
  mempty = Controller $ \_ -> return ()
  mappend a b = Controller $ \r -> unController a r >> unController b r

mkController :: (r -> C r0 s0 r s m ()) -> Controller r0 s0 r s m -- ^
mkController = Controller
{-# INLINABLE mkController #-}

controlling
  :: Monad m
  => Prism' r r'
  -> (s -> Maybe s', s' -> s -> s)
  -> Controller r0 s0 r' s' m
  -> Controller r0 s0 r  s  m -- ^
controlling prr' xss' cer = mkController $ \r ->
    traverse_ (nestController cer xss' (review prr')) (preview prr' r)
{-# INLINABLE controlling #-}


runController
  :: Monad m
  => Controller r s r s m
  -> r
  -> s
  -> Pipes.Producer (Maybe r) m s
runController cer r =
   State.execStateT $ runMaybeT $ runC (unController cer r) id (Just . id) const


--------------------------------------------------------------------------------

-- | A @'C' r0 s0 r s m a@ is where you can issue requests of type @r@ and
-- modify a model state of type @s@, within a larger context of top-level
-- requests of type @r0@ and a top-level model state of type @s0@'. A
-- 'C' where the top-level request and model state types match the request
-- and operation types of the current layer is said to be a top-level 'C'.
-- A 'C' needs be converted into a 'Controller' using 'controller' before
-- it can be used as (or as part of) a top-level 'Controller'.
--
-- * A request is delivered asyncrhonously for another 'Controller' to
--   eventually c it. You use 'req' to issue a request.
--
-- * Changes to the model state happen atomically when the 'Controller' runs.
--
-- * A 'C' (and thus, a 'Controller'), runs until it finishes on its own or
--   until the model state can't be found on the top-level state anymore.
--   This allows top-level 'Controller' to alter their state without causing
--   child controllers to fail.
--
-- 'Controller's can be nested inside a 'C' using the 'nestController' and
-- 'nestController0' combinators, and actions to be executed at the current
-- controller layer can be used by nested 'Controller's after being 'bury'ed.
newtype C r0 s0 r s m a = C
  (    (r -> r0)
    -> (s0 -> Maybe s) -- A getter.
    -> (s -> s0 -> s0) -- A setter. It is OK to leave @s0@ untouched if @s@
                       -- has no place in it.
    -> MaybeT (State.StateT s0 (Pipes.Producer (Maybe r0) m)) a
  ) deriving (Functor)

runC
  :: Monad m
  => C r0 s0 r s m a
  -> (r -> r0)
  -> (s0 -> Maybe s)
  -> (s -> s0 -> s0)
  -> MaybeT (State.StateT s0 (Pipes.Producer (Maybe r0) m)) a
runC (C f) r2r0 gs0s ss0s = do
  s0 <- State.get
  case gs0s s0 of
     Nothing -> MaybeT (return Nothing)
     Just _  -> f r2r0 gs0s ss0s
{-# INLINE runC #-}

instance Monad m => Applicative (C r0 s0 r s m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (C r0 s0 r s m) where
  return a = C $ \_ _ _ -> return a
  ma >>= k = C $ \r2r0 gs0s ss0s -> do
      a <- runC ma r2r0 gs0s ss0s
      runC (k a) r2r0 gs0s ss0s

instance MonadIO m => MonadIO (C r0 s0 r s m) where
  liftIO = lift . liftIO

instance Monad m => State.MonadState s (C r0 s0 r s m) where
  state k = C $ \_ gs0s ss0s -> MaybeT $ State.state $ \s0 ->
    case gs0s s0 of
       Nothing -> (Nothing, s0)
       Just s  -> let !(a, !s') = k s in (Just a, ss0s s' s0)

instance MonadTrans (C r0 s0 r s) where
  lift ma = C $ \_ _ _ -> lift (lift (lift ma))

-- | Issue a local request for another 'Controller' to eventually handle it.
req :: Monad m => r -> C r0 s0 r s m ()
req r = C $ \r2r0 _ _ -> lift $ lift $ Pipes.yield $ Just (r2r0 r)
{-# INLINABLE req #-}

stop :: Monad m => C r0 s0 r s m ()
stop = C $ \_ _ _ -> lift $ lift $ Pipes.yield Nothing
{-# INLINABLE stop #-}

-- | Bury a 'C' so that it can be used at a lower 'C' layer
-- sharing the same top-level request and operation types.
bury
  :: Monad m
  => C r0 s0 r s m a
  -> C r0 s0 r s m (C r0 s0 r' s' m a) -- ^
bury c = C $ \r2r0 gs0s ss0s -> return $ C $ \_ _ _ -> runC c r2r0 gs0s ss0s
{-# INLINABLE bury #-}

-- | Like 'bury' but for a function taking 2 arguments.
--
-- TODO: Not offer this, we should provide just a smarter 'bury' combinator.
bury2
  :: Monad m
  => (y -> z -> C r0 s0 r s m a)
  -> C r0 s0 r s m (y -> z -> C r0 s0 r' s' m a) -- ^
bury2 c = C $ \r2r0 gs0s ss0s -> return $ \y z -> C $ \_ _ _ ->
    runC (c y z) r2r0 gs0s ss0s

-- | Nest a 'Controller' compatible with the same top-level request and
-- model state types.
--
-- Note: using 'nestController0' not only you can obtain a 'C' that can be
-- used inline within other 'Controller', but also you can create a
-- 'Controller' itself that you can then combine with other controller
-- using 'mappend'. The 'controlling' function provides a nicer interface to
-- this way if of composing 'Controller's.
--
-- @
-- mappend (mkController (nestController a b c)) myOtherController
-- @
nestController
  :: Monad m
  => Controller r0 s0 r' s' m
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
nestController cer (gss', sss') r'2r r' = C $ \r2r0 gs0s ss0s -> do
    runC (unController cer r')
         (r2r0 . r'2r)
         (gss' <=< gs0s)
         (\s' s0 -> case gs0s s0 of
             Nothing -> s0
             Just s  -> ss0s (sss' s' s) s0)

-- | Like 'nestController', but for nesting a top-level 'Controller'.
nestController0
  :: (Monad m, Functor m)
  => Controller r' s' r' s' m
  -> (s -> Maybe s', s' -> s -> s)
  -> (r' -> r)
  -> r'
  -> C r0 s0 r s m () -- ^
nestController0 cer (gss', sss') r'2r r' =
    C $ \r2r0 gs0s ss0s -> MaybeT $ State.StateT $ \s0 -> do
       let ms' = gss' =<< gs0s s0
           ss0s' = \s' -> case gs0s s0 of
              Nothing -> s0
              Just s  -> ss0s (sss' s' s) s0
       case ms' of
          Nothing -> return (Nothing, s0)
          Just s' -> do
             s'_ <- runController cer r' s' //> Pipes.yield . fmap (r2r0 . r'2r)
             return (Just (), ss0s' s'_)
{-# INLINABLE nestController0 #-}

--------------------------------------------------------------------------------

newtype VR v r x = VR { unVR :: IO () -> (r -> IO ()) -> v -> IO (x, v) }
  deriving (Functor)

instance Monad (VR v r) where
  return a = VR $ \_ _ v -> return (a, v)
  ma >>= kmb = VR $ \stopIO reqIO v -> do
    (a, v') <- unVR ma stopIO reqIO v
    unVR (kmb a) stopIO reqIO v'

instance Applicative (VR v r) where
  pure = return
  (<*>) = ap

-- | Warning: The 'IO' actions taking in place in 'VR' are intended to prepare
-- the rendering result, but they are not part of the rendering result itself
-- (i.e., the @x@ in @VR v r x@), which means they may not be executed each
-- time the rendering result is needed. If you intend some action to be executed
-- at the time of rendering, then return that action as the result of the @VR@
-- action, that is, have @VR v r (IO ())@ or similar.
instance MonadIO (VR v r) where
  liftIO m = VR $ \_ _ v -> fmap (flip (,) v) m

-- | View state.
instance State.MonadState v (VR v r) where
  state k = VR $ \_ _ v -> return (k v)

-- | Returns an action that will stop the execution of the running application
-- when used. This is comparable to using 'stop' in a 'C'.
--
-- It is safe to keep, share, or call the returned action more than once; it
-- is valid during the whole execution of the Lei application.
getStop :: VR v r (IO ())
getStop = VR $ \a _ v -> return (a, v)

-- | Render a view that had been previously nested using 'nestView'.
render :: ViewRender v r s x -> s -> VR v r x
render vr s = VR $ \stopIO reqIO v -> unVR (unViewRender vr s) stopIO reqIO v

--------------------------------------------------------------------------------

newtype ViewRender v r s x = ViewRender { unViewRender :: s -> VR v r x }

mkViewRenderCacheLast
  :: forall v r s x
   . (Eq v, Eq s)
  => (s -> (r -> IO ()) -> VR v r x)
  -> IO (ViewRender v r s x)
mkViewRenderCacheLast kvr = do
  iorCache <- IORef.newIORef ((\_ _ -> Nothing) :: s -> v -> Maybe (x, v))
  return $ ViewRender $ \s0 -> VR $ \stopIO reqIO v0 -> do
     cacheLookup <- IORef.readIORef iorCache
     case cacheLookup s0 v0 of
        Just xv -> return xv
        Nothing -> do
           !xv@(!_,!_) <- unVR (kvr s0 reqIO) stopIO reqIO v0
           IORef.atomicWriteIORef iorCache $ \s v ->
              if s == s0 && v == v0 then Just xv else Nothing
           return xv

--------------------------------------------------------------------------------

data View v r s m x = View !(ViewInit v m (v, ViewStop v, ViewRender v r s x))

runView :: Monad m => View v r s m x -> m (v, ViewStop v, ViewRender v r s x)
runView (View vi0) = do
   ((v0, vs0, vrr0), vs) <- runViewInit vi0
   return (v0, mappend vs vs0, vrr0)

mkView
  :: (MonadIO m, Eq v, Eq s)
  => ViewInit v m (v, v -> IO (), s -> (r -> IO ()) -> VR v r x)
  -- ^ @v@: view state.
  --
  --   @v -> 'IO' ()@: release resources acquired by 'ViewInit'.
  --
  --   @s@: model state to render.
  --
  --   @r -> 'IO' ()@: issue a request.
  --
  --   @x@: rendering result.
  -> View v r s m x
mkView vi = View $ do
  (v, iovs, kvr) <- vi
  vr <- liftIO $ mkViewRenderCacheLast kvr
  return (v, mkViewStop iovs, vr)

-- Like 'mkView', except for when there is no view state, initialization nor
-- finalization to worry about.
mkViewSimple
  :: (Eq s, MonadIO m)
  => (s -> (r -> IO ()) -> x) -- ^ @s@: state to render.
                              --
                              --   @r -> 'IO' ()@: issue a request.
                              --
                              --   @x@: rendering result.
  -> View () r s m x
mkViewSimple kvr = mkView $ return ((), return, ((return.).) kvr)

--------------------------------------------------------------------------------

newtype ViewInit v m a = ViewInit (State.StateT (ViewStop v) m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

runViewInit :: ViewInit v m a -> m (a, ViewStop v)
runViewInit (ViewInit s) = State.runStateT s mempty

nestView
 :: Monad m
 => Lens' v' v
 -> (r -> r')
 -> View v r s m x
 -> ViewInit v' m (v, ViewRender v' r' s x) -- ^
nestView l r2r' avw = ViewInit $ do
   (av, avs, avrr) <- lift $ runView avw
   State.modify $ mappend (contramapViewStop (view l) avs)
   let vrr = ViewRender $ \s0 -> VR $ \stopIO reqIO v' -> do
               let v1 = view l v'
               (x, v2) <- unVR (unViewRender avrr s0) stopIO (reqIO . r2r') v1
               return (x, set l v2 v')
   return (av, vrr)

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
    tcbDebug <- liftIO $ STM.newBroadcastTChanIO
    let dbgIO = STM.atomically . STM.writeTChan tcbDebug
    -- Channel through which requests are sent
    tqR <- liftIO $ STM.newTQueueIO
    let reqIO = STM.atomically . STM.writeTQueue tqR
    -- TMVar where the last known model state is stored.
    -- 'Right' means it needs rendering.
    tmvSLast <- liftIO $ STM.newTMVarIO (Right s0)
    -- TVar indicating whether to stop execution
    tvStop <- liftIO $ STM.newTVarIO False -- TODO: a `MVar ()` should do
    let stopIO = STM.atomically $ do
          STM.writeTVar tvStop True
          STM.writeTChan tcbDebug DebugStopRequested
    -- Handle an incomming requests
    let handlerLoop :: m ()
        handlerLoop = ($ s0) $ fix $ \loop s -> do
           stopped <- liftIO $ STM.atomically $ STM.readTVar tvStop
           unless stopped $ do
              r <- liftIO $ STM.atomically $ do
                 r <- STM.readTQueue tqR
                 r <$ STM.writeTChan tcbDebug (DebugReqStart r s)
              !s' <- Pipes.runEffect $ do
                 Pipes.for (runController cer r s) $ \mr' -> do
                    case mr' of
                       Just r' -> liftIO $ reqIO r'
                       Nothing -> liftIO stopIO >> lift (loop s)
              liftIO $ STM.atomically $ do
                 void $ STM.tryTakeTMVar tmvSLast
                 if s == s'
                    then do STM.putTMVar tmvSLast (Left s')
                            STM.writeTChan tcbDebug $ DebugStateSame s'
                    else do STM.putTMVar tmvSLast (Right s')
                            STM.writeTChan tcbDebug $ DebugStateNew s'
              loop s'
    -- Render the model
    let renderLoop :: v -> ViewStop v -> ViewRender v r s (IO ()) -> IO ()
        renderLoop v0 vs vrr = ($ v0) $ fix $ \loop v -> do
           flip Ex.onException (runViewStop vs v) $ do
              stopped <- STM.atomically $ STM.readTVar tvStop
              if stopped
                 then runViewStop vs v
                 else do
                    Ex.bracketOnError
                       (STM.atomically $ do
                           es <- STM.takeTMVar tmvSLast
                           case es of
                              Left _ -> STM.retry
                              Right s -> do
                                STM.writeTChan tcbDebug (DebugRenderStart v s)
                                return s)
                       (\s -> STM.atomically $ do
                           void $ STM.tryPutTMVar tmvSLast $ Right s)
                       (\s -> do
                           (io, v') <- unVR (unViewRender vrr s) stopIO reqIO v
                           io >> loop v')
        debugging :: forall x. m x -> m x
        debugging k = bracket
           (liftIO $ do
              tcbDebug' <- STM.atomically $ STM.dupTChan tcbDebug
              Async.async $ dbg0 $ STM.atomically $ STM.readTChan tcbDebug')
           (liftIO . Async.cancel)
           (\_ -> k)

    debugging $ bracket
       (do (v, vs, vrr) <- runView vw
           liftIO $ dbgIO $ DebugViewInitialized v
           liftIO $ flip Ex.onException (runViewStop vs v) $ do
              a1 <- Async.async $ renderLoop v vs vrr
              a1 <$ Async.link a1)
       (liftIO . Async.cancel)
       (\a1 -> handlerLoop >> liftIO (Async.wait a1))


data Debug v r s
  = DebugStopRequested
  | DebugViewInitialized v
  | DebugRenderStart v s
  | DebugReqStart r s
  | DebugStateNew s
  | DebugStateSame s
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

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
               (traverse_ ($ v))

-- | @'mappend' a b@ runs @a@ first and then @b@, even in case of exceptions.
instance Monoid (ViewStop v) where
  mempty = mkViewStop $ const $ return ()
  mappend a b = mkViewStop $ \v -> runViewStop a v `Ex.finally` runViewStop b v

contramapViewStop :: (a -> b) -> ViewStop b -> ViewStop a
contramapViewStop f x = mkViewStop $ runViewStop x . f
