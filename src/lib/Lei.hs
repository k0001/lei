{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | > import qualified Lei
module Lei
  ( run

  -- * Model
  , Model
  , mkModel
  , runModel

  -- * Controller
  , Controller
  , mkController

  , C
  , req
  , op
  , stop
  , bury
  , nestController
  , nestController0

  -- * View
  , View
  , mkView
  , mkView_
  , nestView

  , ViewStop
  , mkViewStop

  , ViewRender
  , runViewRender

  -- * Debug
  , Debug(..)
  ) where

import           Control.Applicative
import qualified Control.Exception as Ex
import           Control.Concurrent.MVar as MVar
import           Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as STM
import           Control.Monad (void, unless)
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Control.Monad.Trans.Class (MonadTrans(lift))
import qualified Control.Monad.Trans.State as State
import           Data.Foldable (foldl', traverse_)
import           Data.Function (fix)
import           Data.Functor.Identity (Identity(..))
import           Data.Monoid (Monoid(..))
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Pipes
import           Pipes.Core ((//>))
import           Prelude hiding (sequence_)


--------------------------------------------------------------------------------

newtype Model o s = Model { runModel :: o -> s -> s }

mkModel :: (o -> s -> s) -> Model o s
mkModel = Model

--------------------------------------------------------------------------------

-- | A 'Controller', constructed with 'controller', is where you deal with a
-- concrete request of type @r@ using a given @'C' r0 o0 r o m a@.
newtype Controller r0 o0 r o s m
  = Controller { unController :: s -> r -> C r0 o0 r o m () }

instance Monad m => Monoid (Controller r0 o0 r o s m) where
  mempty = Controller $ \_ _ -> return ()
  mappend a b = Controller $ \s r -> unController a s r >> unController b s r

mkController :: (s -> r -> C r0 o0 r o m ()) -> Controller r0 o0 r o s m
mkController = Controller

-- | Run a 'Controller'.
runController
  :: Monad m
  => Controller r0 o0 r o s m
  -> (r -> r0)
  -> (o -> o0)
  -> s
  -> r
  -> Pipes.Producer (Maybe r0) m (Seq o0)
runController cer r2r0 o2o0 s r = Pipes.hoist
    (flip State.evalStateT mempty)
    (runC (unController cer s r) r2r0 o2o0 >> lift State.get)

--------------------------------------------------------------------------------

-- | A @'C' r0 o0 r o m a@ is where you can issue requests of type @r@ and
-- operations of type @o@, within a larger context of top-level requests of type
-- @r0@ possibly leading to the issue of top-level operations of type @o0@'. A
-- 'C' where the top-level request and operation types match the request
-- and operation types of the current layer is said to be a top-level 'C'.
-- A 'C' needs be converted into a 'Controller' using 'controller' before
-- it can be used as (or as part of) a top-level 'Controller'.
--
-- * A request is delivered asyncrhonously for another 'Controller' to
--   eventually c it. You use 'req' to issue a request.
--
-- * An operation is delivered synchronously for a model to update. You use 'op'
--   to issue an operation.
--
-- 'Controller's can be nested inside a 'C' using the 'nest' and 'nest0'
-- combinators, and actions to be executed at the current controller layer can
-- be used by nested 'Controller's after being 'bury'ed.
newtype C r0 o0 r o m a = C
  { runC :: (r -> r0)
         -> (o -> o0)
         -> Pipes.Producer (Maybe r0) (State.StateT (Seq o0) m) a }
  deriving (Functor)

instance Monad m => Applicative (C r0 o0 r o m) where
  pure a = C $ \_ _ -> pure a
  mf <*> ma = C $ \r2r0 o2o0 ->
      runC mf r2r0 o2o0 <*> runC ma r2r0 o2o0

instance Monad m => Monad (C r0 o0 r o m) where
  return a = C $ \ _ _ -> return a
  ma >>= k = C $ \r2r0 o2o0 -> do
      a <- runC ma r2r0 o2o0
      runC (k a) r2r0 o2o0

instance MonadTrans (C r0 o0 r o) where
  lift ma = C $ \_ _ -> lift (lift ma)

instance MonadIO m => MonadIO (C r0 o0 r o m) where
  liftIO = lift . liftIO

-- | Issue a local request for another 'Controller' to eventually handle it.
req :: Monad m => r -> C r0 o0 r o m ()
req r = C $ \r2r0 _ -> Pipes.yield $ Just (r2r0 r)

-- | Issue a local operation for a model to update.
-- All the given operations will be applied atomically.
op :: Monad m => o -> C r0 o0 r o m ()
op o = C $ \_ o2o0 -> lift $ State.modify (Seq.|> o2o0 o)

stop :: Monad m => C r0 o0 r o m ()
stop = C $ \_ _ -> Pipes.yield Nothing

-- | Bury a 'C' so that it can be used at a lower 'C' layer
-- sharing the same top-level request and operation types.
bury
  :: Monad m
  => C r0 o0 r o m a
  -> C r0 o0 r o m (C r0 o0 r' o' m a)
bury c = C $ \r2r0 o2o0 -> return (C $ \_ _ -> runC c r2r0 o2o0)

-- | Nest a 'Controller' compatible with the same top-level request
-- and operation types.
nestController
  :: Monad m
  => s
  -> r
  -> (r -> r')
  -> (o -> o')
  -> Controller r0 o0 r o s m
  -> C r0 o0 r' o' m ()
nestController s r r2r' o2o' cer = C $ \r'2r0 o'2o0 ->
    runC (unController cer s r) (r'2r0 . r2r') (o'2o0 . o2o')

-- | Nest a top-level 'Controller'.
nestController0
  :: (Monad m, Functor m)
  => s
  -> r
  -> Controller r o r o s m
  -> C r0 o0 r o m ()
nestController0 s r cer = C $ \r2r0 o2o0 -> Pipes.hoist
    (\x -> State.StateT $ \os' -> do
       (a, os) <- State.runStateT x mempty
       return $ (,) a $! mappend os' (fmap o2o0 os))
    (runC (unController cer s r) id id //> Pipes.yield . fmap r2r0)

--------------------------------------------------------------------------------

newtype ViewRender v r s x = ViewRender
  { runViewRender :: IO () -> (r -> IO ()) -> s -> State.State v x }

--------------------------------------------------------------------------------

data View v r s m x = View !(ViewInit v m (v, ViewStop v, ViewRender v r s x))

runView :: Monad m => View v r s m x -> m (v, ViewStop v, ViewRender v r s x)
runView (View vi0) = do
   ((v0, vs0, vr0), vs) <- runViewInit vi0
   return (v0, mappend vs vs0, vr0)

mkView
  :: Monad m
  => ViewInit v m (v,
                   v -> IO (),
                   IO () -> (r -> IO ()) -> s -> State.State v x)
  -> View v r s m x -- ^
mkView vi = View $ do
  (v, iovs, k) <- vi
  return (v, mkViewStop iovs, ViewRender k)

-- Like 'mkView', except for when there is no view state, initialization nor
-- finalization to worry about.
mkView_
 :: Monad m
 => (IO () -> (r -> IO ()) -> s -> x)
 -> View () r s m x -- ^
mkView_ k =
    let vs = \() -> return ()
        vr = \stopIO reqIO s -> return $! k stopIO reqIO s
    in mkView $ return ((), vs, vr)

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
 -> ViewInit v' m (v, ViewRender v' r' s x)
nestView l r2r' avw = ViewInit $ do
   (av, avs, avr) <- lift $ runView avw
   State.modify $ mappend (contramapViewStop (lensView l) avs)
   let vr = ViewRender $ \vs' reqIO s ->
               lensZoom l $ runViewRender avr vs' (reqIO . r2r') s
   return (av, vr)

--------------------------------------------------------------------------------

-- | Run a Lei application.
--
run
  :: forall r o v s m a
   . (MonadIO m, Eq s)
  => (forall x y z . m x -> (x -> m y) -> (x -> m z) -> m z)
  -- ^ 'Control.Exception.bracket'-like function.
  --    Hint: use 'Control.Monad.Catch.bracket' from "Control.Monad.Catch".
  -> (IO (Debug v r o s) -> IO a)
  -- ^ Loop monitoring events.
  -> s
  -- ^ Initial model state.
  -> Model o s
  -- ^ /Model/ update function. See 'model'.
  -> Controller r o r o s m
  -- ^ /Controller/ issuing controller requests and model operations.
  -- See 'controller'.
  -> View v r s m (IO ())
     -- ^ /View/ issuing controller requests and rendering the model.
  -> m ()
run bracket dbg0 s0 m cer vw = do
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
              os <- Pipes.runEffect $ do
                 Pipes.for (runController cer id id s r) $ \mr' -> do
                    case mr' of
                       Just r' -> liftIO $ reqIO r'
                       Nothing -> liftIO stopIO >> lift (loop s)
              let !s' = foldl' (flip (runModel m)) s os
              liftIO $ STM.atomically $ do
                 void $ STM.tryTakeTMVar tmvSLast
                 if s == s'
                    then do STM.putTMVar tmvSLast (Left s')
                            STM.writeTChan tcbDebug $ DebugStateSame os s'
                    else do STM.putTMVar tmvSLast (Right s')
                            STM.writeTChan tcbDebug $ DebugStateNew os s'
              loop s'
    -- Render the model
    let renderLoop :: v -> ViewStop v -> ViewRender v r s (IO ()) -> IO ()
        renderLoop v0 vs vr = ($ v0) $ fix $ \loop v -> do
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
                           let st = runViewRender vr stopIO reqIO s
                               (io, v') = State.runState st v
                           io >> loop v')
        debugging :: forall x. m x -> m x
        debugging k = bracket
           (liftIO $ do
              tcbDebug' <- STM.atomically $ STM.dupTChan tcbDebug
              Async.async $ dbg0 $ STM.atomically $ STM.readTChan tcbDebug')
           (liftIO . Async.cancel)
           (\_ -> k)

    debugging $ bracket
       (do (v, vs, vr) <- runView vw
           liftIO $ dbgIO $ DebugViewInitialized v
           liftIO $ flip Ex.onException (runViewStop vs v) $ do
              a1 <- Async.async $ renderLoop v vs vr
              a1 <$ Async.link a1)
       (liftIO . Async.cancel)
       (\a1 -> handlerLoop >> liftIO (Async.wait a1))


data Debug v r o s
  = DebugStopRequested
  | DebugViewInitialized v
  | DebugRenderStart v s
  | DebugReqStart r s
  | DebugStateNew (Seq o) s
  | DebugStateSame (Seq o) s
  deriving (Show)

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

--------------------------------------------------------------------------------
-- The wheel, mon ami, it is necessary to reinvent it.

type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

lensSet :: Lens' s a -> a -> s -> s
lensSet l b = runIdentity . l (\_ -> Identity b)

lensView :: Lens' s a -> s -> a
lensView l = getConst . l Const

lensZoom :: Lens' a b -> State.State b x -> State.State a x
lensZoom l sb = State.state $ \a ->
    let (x, b) = State.runState sb (lensView l a)
    in (x, lensSet l b a)
