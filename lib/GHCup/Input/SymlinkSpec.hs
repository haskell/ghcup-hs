{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module GHCup.Input.SymlinkSpec where

import GHCup.Errors
import GHCup.Input.Parsers.Domain
import GHCup.Prelude.Logger
import GHCup.Query.GHCupDirs
import GHCup.Types
import GHCup.Types.Optics

import Control.Monad                ( forM )
import Control.Monad.IO.Class       ( liftIO )
import Control.Monad.Reader         ( MonadReader )
import Data.Variant.Excepts         ( Excepts, throwE )
import Data.Versions                ( Version, prettyVer )
import System.FilePath              ( takeFileName )
import System.FilePattern.Directory ( getDirectoryFilesIgnore )

import qualified Data.Map.Strict as M
import qualified Data.Text       as T
import qualified Text.Megaparsec as MP


parseSymlinkSpec ::
    Monad m
  => Version
  -> SymlinkSpec String
  -> Excepts
       '[ParseError]
       m
       (SymlinkSpec [Either Char Version])
parseSymlinkSpec ver SymlinkSpec{..} = do
  target <- either (throwE . ParseError . show) pure $ parseDomain _slTarget
  linkName <- either (throwE . ParseError . show) pure $ parseDomain _slLinkName
  setName <- forM _slSetName $ either (throwE . ParseError . show) pure . parseDomain
  pure $ SymlinkSpec target linkName _slPVPMajorLinks setName
 where
   parseDomain = MP.parse (domainParserVer env) "parseSymlinkSpec"
   env = M.fromList [(PKGVER, T.unpack $ prettyVer ver)
                    ]


substituteSpec :: SymlinkSpec [Either Char Version] -> SymlinkFileSpec
substituteSpec SymlinkSpec{..} =
  let target = mconcat $ either (:[]) (T.unpack . prettyVer) <$> _slTarget
      linkName = mconcat $ either (:[]) (T.unpack . prettyVer) <$> _slLinkName
      setName = mconcat . fmap (either (:[]) (T.unpack . prettyVer)) <$> _slSetName
  in SymlinkSpec target linkName _slPVPMajorLinks setName


resolveSymlinkSpec  ::
     ( MonadReader env m
     , HasLog env
     , MonadIOish m
     )
  => GHCupPath
  -> SymlinkInputSpec
  -> Excepts
       '[ParseError]
       m
       [SymlinkSpec String]
resolveSymlinkSpec _ SymlinkInputSpec{..} = pure [SymlinkSpec{..}]
resolveSymlinkSpec (fromGHCupPath -> workDir) SymlinkPatternSpec{..} = do
  binaries <- liftIO $ getDirectoryFilesIgnore workDir _slTargetPattern _slTargetPatternIgnore
  logDebug2 $ "Resolved binaries found in " <> T.pack workDir <> ": " <> T.pack (show binaries)
  forM binaries $ \binary -> do
    let _slTarget = binary
    _slLinkName <- parseDomain (takeFileName binary) _slLinkName
    logDebug2 $ "Parsed slLinkName: " <> T.pack _slLinkName
    _slSetName <- forM _slSetName $ parseDomain (takeFileName binary)
    logDebug2 $ "Parsed slSetName: " <> T.pack (show _slSetName)
    pure SymlinkSpec{..}
 where
   parseDomain targetFn = throwOnParseError . MP.parse (domainParser' env [PKGVER]) "resolveSymlinkSpec"
    where
     env = M.fromList [(TARGETFN, targetFn)
                      ]

