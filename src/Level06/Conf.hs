{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Level06.Conf
  ( parseOptions,
  )
where

import Data.Bifunctor (first)
import Data.Semigroup (Last (..), Semigroup ((<>)))
import GHC.Word (Word16)
import Level06.AppM (AppM, liftEither)
import Level06.Conf.CommandLine (commandLineParser)
import Level06.Conf.File (parseJSONConfigFile)
import Level06.Types
  ( Conf(..),
    ConfigError(..),
    DBFilePath (DBFilePath),
    PartialConf(..),
    Port (Port),
  )
import           Control.Monad.IO.Class

-- | For the purposes of this application we will encode some default values to
-- ensure that our application continues to function in the event of missing
-- configuration values from either the file or command line inputs.
defaultConf ::
  PartialConf
defaultConf =
  PartialConf {
    pcPort = Just (Last $ Port 3000),
    pcDBFilePath = Just (Last $ DBFilePath "app_db.db")
  }

-- | We need something that will take our PartialConf and see if can finally build
-- a complete ``Conf`` record. Also we need to highlight any missing values by
-- providing the relevant error.
makeConfig ::
  PartialConf ->
  Either ConfigError Conf
makeConfig partialConf =
    case (maybePort, maybeDBFilePath) of
      (Just (Last port), Just (Last dbFilePath)) ->
        Right $ Conf port dbFilePath
      (Nothing, Just (Last _dbFilePath)) ->
        Left $ IncompleteConfError "Missing port"
      (Just (Last _port), Nothing) ->
        Left $ IncompleteConfError "Missing DB file path"
      (Nothing, Nothing) ->
        Left $ IncompleteConfError "Missing port and DB file path"
  where
    maybePort = pcPort partialConf
    maybeDBFilePath = pcDBFilePath partialConf

-- | This is the function we'll actually export for building our configuration.
-- Since it wraps all our efforts to read information from the command line, and
-- the file, before combining it all and returning the required information.
--
-- Remember that we want the command line configuration to take precedence over
-- the File configuration, so if we think about combining each of our ``Conf``
-- records. By now we should be able to write something like this:
--
-- ``defaults <> file <> commandLine``
parseOptions ::
  FilePath ->
  AppM ConfigError Conf
parseOptions fp =
  getPartialConfig >>= liftEither . makeConfig
  where
    getPartialConfig :: AppM ConfigError PartialConf
    getPartialConfig = getFileConfig >>= (\fileConfig -> getCommandLine >>= (\commandLineConfig -> pure $ defaultConf <> fileConfig <> commandLineConfig))
    getFileConfig :: AppM ConfigError PartialConf
    getFileConfig = parseJSONConfigFile fp
    getCommandLine :: AppM ConfigError PartialConf
    getCommandLine = liftIO commandLineParser
