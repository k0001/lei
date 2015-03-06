{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import           Control.Lens ((.=), (%=), (-=), makeLenses)
import           Control.Applicative
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Ex
import           Control.Monad (forever, forM_, guard, void)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.State as State
import           Data.Foldable (traverse_)
import qualified Lei
import           Prelude

--------------------------------------------------------------------------------
-- THE MODEL

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


-- | The state of our application.
data Model = Model
  { _appStatus      :: !PingPong
  , _appChangesLeft :: !Integer
  , _appErrors      :: ![Error]
  , _appStop        :: !Bool
  } deriving (Eq, Show)

makeLenses ''Model

-- | @'def' n@ create an 'Model' starting on a 'Ping' state that allows up to
-- @n@ changes, where @n > 0@.
def :: Integer -> Maybe Model
def n = do
    guard $ n > 0
    return $ Model Ping n [] False

--------------------------------------------------------------------------------
-- THE VIEW

view :: MonadIO m => Env -> Lei.View () Req Model m (IO ())
view e = Lei.mkViewSimple $ \req s -> do
       printReport s
       envPrompter e $ do
          a <- readPingPong
          req $ ReqSetStatus a
  where
    msg :: String -> IO ()
    msg a = putStrLn $ "| " ++ a

    printReport :: Model -> IO ()
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

controller :: MonadIO m => Lei.Controller r0 s0 Req Model m
controller = Lei.mkController $ \r -> do
  appErrors .= []
  case r of
    ReqStop -> appStop .= True
    ReqSetStatus dst -> do -- TODO: MonadPlus instance for Lei.C
      s <- State.get
      let errors = concat
            [ [ErrorNoChangesLeft | _appChangesLeft s < 1]
            , [ErrorCantSwap Ping | _appStatus s /= pingPongToggle dst]
            ]
      case errors of
        [] -> do
           appStatus %= pingPongToggle
           appChangesLeft -= 1
        _  -> appErrors %= (errors ++)

--------------------------------------------------------------------------------

main :: IO ()
main = do
    Just s0 <- return $ def 5
    venv <- newEnv
    Lei.run Ex.bracket (\_ -> return ()) s0 controller (view venv)

--------------------------------------------------------------------------------
-- Internal tools

readLnMay :: Read a => IO (Maybe a)
readLnMay = do
   l <- getLine
   return $ case reads l of
     [(a,"")] -> Just a
     _        -> Nothing
