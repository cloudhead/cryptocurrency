{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Bitcoin.Log
    ( LogLevel
    , Logger
    , logL
    , infoL
    , debugL
    , toLog
    ) where

import           System.Log.FastLogger
import           System.IO.Unsafe (unsafePerformIO)
import           Control.Monad.IO.Class
import           Data.Monoid ((<>))

data LogLevel = DEBUG | INFO | ERROR | WARNING deriving (Eq, Show)

type Logger = forall a m. (Monad m, ToLogStr a) => a -> m ()

toLog :: Show a => a -> LogStr
toLog = toLogStr . show

stdoutLogger :: LoggerSet
stdoutLogger = unsafePerformIO $ newStdoutLoggerSet defaultBufSize

stderrLogger :: LoggerSet
stderrLogger = unsafePerformIO $ newStderrLoggerSet defaultBufSize

debugL :: (ToLogStr a, MonadIO m) => a -> m ()
debugL = logL DEBUG

infoL :: (ToLogStr a, MonadIO m) => a -> m ()
infoL = logL INFO

_errorL :: (ToLogStr a, MonadIO m) => a -> m ()
_errorL = logL ERROR

logHeader :: LogStr
logHeader = "[bitcoin]"

logL :: (ToLogStr a, MonadIO m) => LogLevel -> a -> m ()
logL lvl msg = do
    liftIO $ pushLogStr logger $
        logHeader <> " "
                  <> toLogStr (show lvl) <> " "
                  <> toLogStr msg
                  <> "\n"
  where
    logger = if lvl == ERROR
             then stderrLogger
             else stdoutLogger

