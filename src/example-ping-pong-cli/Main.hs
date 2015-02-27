{-# LANGUAGE LambdaCase #-}

module Main (main) where

import           Control.Applicative
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Ex
import           Control.Monad (forever, forM_, guard, void)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Foldable (traverse_)
import qualified Lei
import           Prelude

--------------------------------------------------------------------------------
-- THE MODEL

-- | The state of our application.
data PingPongState = PingPongState
  { _appStatus      :: !PingPong
  , _appChangesLeft :: !Integer
  , _appErrors      :: ![Error]
  , _appStop        :: !Bool
  } deriving (Eq, Show)

-- | @'def' n@ create an 'PingPongState' starting on a 'Ping' state that allows up to
-- @n@ changes, where @n > 0@.
def :: Integer -> Maybe PingPongState
def n = do
    guard $ n > 0
    return $ PingPongState Ping n [] False


data PingPong = Ping | Pong
  deriving (Eq, Show, Read)

pingPongToggle :: PingPong -> PingPong
pingPongToggle = \case
    Ping -> Pong
    Pong -> Ping


data Error
  = ErrorNoChangesLeft
  | ErrorCantSwap !PingPong
  deriving (Eq, Show)


-- | Operations that will, undeniably, change our 'PingPongState'.
data Op
  = OpAddError Error
  | OpClearErrors
  | OpSwapStatus
  | OpStop
  deriving (Show)

model :: Lei.Model Op PingPongState
model = Lei.mkModel $ \o s -> case o of
    OpStop -> s { _appStop = True }
    OpAddError e -> s { _appErrors = e : _appErrors s }
    OpClearErrors -> s { _appErrors = [] }
    OpSwapStatus -> s { _appStatus = pingPongToggle $ _appStatus s
                      , _appChangesLeft = _appChangesLeft s - 1
                      }

--------------------------------------------------------------------------------
-- THE VIEW

view :: MonadIO m => Env -> Lei.View () Req PingPongState m (IO ())
view e = Lei.mkView_ $ \s -> do
    req <- Lei.vrReq
    return $ do
       printReport s
       envPrompter e $ do
          a <- readPingPong
          req $ ReqSetStatus a
  where
    msg :: String -> IO ()
    msg a = putStrLn $ "| " ++ a

    printReport :: PingPongState -> IO ()
    printReport s = do
       forM_ (_appErrors s) $ \e ->
          msg $ "ERROR: " ++ errorHumanReadable e
       msg $ "Current status: " ++ show (_appStatus s)
       msg $ "Number of status changes left: " ++ show (_appChangesLeft s)
       msg $ "Try typing " ++ show (pingPongToggle (_appStatus s))

    readPingPong :: IO PingPong
    readPingPong = do
       ma <- readLnMay
       case ma of
          Just a  -> return a
          Nothing -> do
             msg $ "Oops, that's unacceptable, try typing Ping or Pong instead."
             readPingPong

errorHumanReadable :: Error -> String
errorHumanReadable = \case
    ErrorNoChangesLeft -> "You can't make any more changes."
    ErrorCantSwap a -> "You can't switch to " ++ show a ++ " now."


-- | This is supposed to be an “environment” providing tools that are
-- requiered to properly render the view and such. In this case, it is a simple
-- “UI toolkit” providing a tool that repeatedly prompts the user for something.
data Env = Env
  { envStop :: IO ()
  , envPrompter :: IO () -> IO ()
  }

newEnv :: IO Env
newEnv = do
   lock <- MVar.newMVar ()
   mvAsyncId <- MVar.newEmptyMVar
   let killPrompter = traverse_ Async.cancel =<< MVar.tryTakeMVar mvAsyncId
       stop = MVar.withMVar lock $ \() ->
         killPrompter
       prompter m = MVar.withMVar lock $ \() -> do
         killPrompter
         MVar.putMVar mvAsyncId =<< Async.async (forever m)
   return (Env stop prompter)

--------------------------------------------------------------------------------
-- THE CONTROLLER

-- | Requests that can be made to our application.
data Req
  = ReqStop
  | ReqSetStatus !PingPong
  deriving (Show)

controller :: Monad m => Lei.Controller r0 o0 Req Op PingPongState m
controller = Lei.mkController $ \s -> \case
    ReqStop -> Lei.op OpStop
    ReqSetStatus dst -> do -- TODO: MonadPlus instance for Lei.C
      Lei.op OpClearErrors
      let errors = concat
            [ [ErrorNoChangesLeft | _appChangesLeft s < 1]
            , [ErrorCantSwap Ping | _appStatus s /= pingPongToggle dst]
            ]
      case errors of
        [] -> Lei.op OpSwapStatus
        _  -> forM_ errors $ Lei.op . OpAddError

--------------------------------------------------------------------------------

main :: IO ()
main = do
    Just s0 <- return $ def 5
    venv <- newEnv
    Lei.run Ex.bracket (\_ -> return ()) s0 model controller (view venv)

--------------------------------------------------------------------------------
-- Internal tools

readLnMay :: Read a => IO (Maybe a)
readLnMay = do
   l <- getLine
   return $ case reads l of
     [(a,"")] -> Just a
     _        -> Nothing
