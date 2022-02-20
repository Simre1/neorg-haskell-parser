{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}

module Effect.Logging where

import Cleff
import Data.Text (Text)
import qualified Data.Text.IO as T
import System.IO (stderr)

data Logging :: Effect where
  LogError :: Text -> Logging m ()
  LogWarning :: Text -> Logging m ()
  LogInfo :: Text -> Logging m ()

makeEffect ''Logging

stdErrorLogging :: IOE :> es => Eff (Logging ': es) ~> Eff es
stdErrorLogging = interpretIO $ \case
  LogError t -> T.hPutStrLn stderr "Error: " >> T.hPutStrLn stderr t
  LogWarning t -> T.hPutStrLn stderr "Warning: " >> T.hPutStrLn stderr t
  LogInfo t -> T.hPutStrLn stderr "Info: " >> T.hPutStrLn stderr t


ignoreLogging :: Eff (Logging ': es) ~> Eff es
ignoreLogging = interpret $ \case
  LogError _ -> pure ()
  LogWarning _ -> pure ()
  LogInfo t -> pure ()
