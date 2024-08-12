{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}


module GHCup.OptParse (
    module GHCup.OptParse.Common
  , module GHCup.OptParse.Install
  , module GHCup.OptParse.Test
  , module GHCup.OptParse.Set
  , module GHCup.OptParse.UnSet
  , module GHCup.OptParse.Rm
  , module GHCup.OptParse.Compile
  , module GHCup.OptParse.Config
  , module GHCup.OptParse.Whereis
  , module GHCup.OptParse.List
#ifndef DISABLE_UPGRADE
  , module GHCup.OptParse.Upgrade
#endif
  , module GHCup.OptParse.ChangeLog
  , module GHCup.OptParse.Prefetch
  , module GHCup.OptParse.GC
  , module GHCup.OptParse.DInfo
  , module GHCup.OptParse.Nuke
  , module GHCup.OptParse.ToolRequirements
  , module GHCup.OptParse.Run
  , module GHCup.OptParse
) where


import           GHCup.OptParse.Common
import           GHCup.OptParse.Install
import           GHCup.OptParse.Test
import           GHCup.OptParse.Set
import           GHCup.OptParse.UnSet
import           GHCup.OptParse.Rm
import           GHCup.OptParse.Run
import           GHCup.OptParse.Compile
import           GHCup.OptParse.Config
import           GHCup.OptParse.Whereis
import           GHCup.OptParse.List
#ifndef DISABLE_UPGRADE
import           GHCup.OptParse.Upgrade
#endif
import           GHCup.OptParse.ChangeLog
import           GHCup.OptParse.Prefetch
import           GHCup.OptParse.GC
import           GHCup.OptParse.DInfo
import           GHCup.OptParse.ToolRequirements
import           GHCup.OptParse.Nuke

import           GHCup.Types
import           GHCup.Utils.Parsers (gpgParser, downloaderParser, keepOnParser, platformParser, parseUrlSource)
#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Control.Monad.Reader
import           Data.Either
import           Data.Functor
import           Data.Maybe
import           Options.Applicative     hiding ( style )
import           Options.Applicative.Help.Pretty ( text )
import           Prelude                 hiding ( appendFile )



data Options = Options
  {
  -- global options
    optVerbose     :: Maybe Bool
  , optCache       :: Maybe Bool
  , optMetaCache   :: Maybe Integer
  , optMetaMode    :: Maybe MetaMode
  , optPlatform    :: Maybe PlatformRequest
  , optUrlSource   :: Maybe URLSource
  , optNoVerify    :: Maybe Bool
  , optKeepDirs    :: Maybe KeepDirs
  , optsDownloader :: Maybe Downloader
  , optNoNetwork   :: Maybe Bool
  , optGpg         :: Maybe GPGSetting
  , optStackSetup  :: Maybe Bool
  , optPager       :: Maybe Bool
  -- commands
  , optCommand     :: Command
  }

data Command
  = Install (Either InstallCommand InstallOptions)
  | Test TestCommand
  | InstallCabalLegacy InstallOptions
  | Set (Either SetCommand SetOptions)
  | UnSet UnsetCommand
  | List ListOptions
  | Rm (Either RmCommand RmOptions)
  | DInfo
  | Compile CompileCommand
  | Config ConfigCommand
  | Whereis WhereisOptions WhereisCommand
#ifndef DISABLE_UPGRADE
  | Upgrade UpgradeOpts Bool Bool
#endif
  | ToolRequirements ToolReqOpts
  | ChangeLog ChangeLogOptions
  | Nuke
#if defined(BRICK)
  | Interactive
#endif
  | Prefetch PrefetchCommand
  | GC GCOptions
  | Run RunOptions
  | PrintAppErrors



opts :: Parser Options
opts =
  Options
    <$> invertableSwitch "verbose" (Just 'v') False (help "Enable verbosity (default: disabled)")
    <*> invertableSwitch "cache" (Just 'c') False (help "Cache downloads in ~/.ghcup/cache (default: disabled)")
    <*> optional (option auto (long "metadata-caching" <> metavar "SEC" <> help "How long the yaml metadata caching interval is (in seconds), 0 to disable"))
    <*> optional (option auto (long "metadata-fetching-mode" <> metavar "<Strict|Lax>" <> help "Whether to fail on metadata download failure (Strict) or fall back to cached version (Lax (default))"))
    <*> optional
      (option
        (eitherReader platformParser)
        (  short 'p'
        <> long "platform"
        <> metavar "PLATFORM"
        <> help
             "Override for platform (triple matching ghc tarball names), e.g. x86_64-fedora27-linux"
        )
      )
    <*> optional
          (option
            (eitherReader parseUrlSource)
            (  short 's'
            <> long "url-source"
            <> metavar "URL_SOURCE"
            <> help "Alternative ghcup download info"
            <> internal
            <> completer urlSourceCompleter
            )
          )
    <*> (fmap . fmap) not (invertableSwitch "verify" (Just 'n') True (help "Disable tarball checksum verification (default: enabled)"))
    <*> optional (option
          (eitherReader keepOnParser)
          (  long "keep"
          <> metavar "<always|errors|never>"
          <> help
               "Keep build directories? (default: errors)"
          <> hidden
          <> completer (listCompleter ["always", "errors", "never"])
          ))
    <*> optional (option
          (eitherReader downloaderParser)
          (  long "downloader"
#if defined(INTERNAL_DOWNLOADER)
          <> metavar "<internal|curl|wget>"
          <> help
          "Downloader to use (default: internal)"
          <> completer (listCompleter ["internal", "curl", "wget"])
#else
          <> metavar "<curl|wget>"
          <> help
          "Downloader to use (default: curl)"
          <> completer (listCompleter ["curl", "wget"])
#endif
          <> hidden
          ))
    <*> invertableSwitch "offline" (Just 'o') False (help "Don't do any network calls, trying cached assets and failing if missing.")
    <*> optional (option
          (eitherReader gpgParser)
          (  long "gpg"
          <> metavar "<strict|lax|none>"
          <> help
          "GPG verification (default: none)"
          <> completer (listCompleter ["strict", "lax", "none"])
          ))
    <*> invertableSwitch "stack-setup" Nothing False (help "Use stack's setup info for discovering and installing GHC versions")
    <*> (invertableSwitch "paginate" Nothing False (help "Send output (e.g. from 'ghcup list') through pager (default: disabled)"))
    <*> com



com :: Parser Command
com =
  subparser
#if defined(BRICK)
      (  command
          "tui"
          (   (\_ -> Interactive)
          <$> info
                helper
                (  progDesc "Start the interactive GHCup UI"
                )
          )
      <>  command
#else
      (  command
#endif
          "install"
          (   Install
          <$> info
                (installParser <**> helper)
                (  progDesc "Install or update GHC/cabal/HLS/stack"
                <> footerDoc (Just $ text installToolFooter)
                )
          )
      <> command
           "test"
           (info
             (Test <$> testParser <**> helper)
             (  progDesc "Run tests for a tool (if any) [EXPERIMENTAL!]"
             <> footerDoc (Just $ text testFooter)
             )
           )
      <> command
           "set"
           (info
             (Set <$> setParser <**> helper)
             (  progDesc "Set currently active GHC/cabal version"
             <> footerDoc (Just $ text setFooter)
             )
           )
      <> command
           "unset"
           (info
             (UnSet <$> unsetParser <**> helper)
             (  progDesc "Unset currently active GHC/cabal version"
             <> footerDoc (Just $ text unsetFooter)
             )
           )
      <> command
           "rm"
           (info
             (Rm <$> rmParser <**> helper)
             (  progDesc "Remove a GHC/cabal/HLS/stack version"
             <> footerDoc (Just $ text rmFooter)
             )
           )

      <> command
           "list"
           (info (List <$> listOpts <**> helper)
                 (progDesc "Show available GHCs and other tools"
                 <> footerDoc (Just $ text listToolFooter))
           )
      <> command
           "upgrade"
           (info
             (    (Upgrade <$> upgradeOptsP <*> switch
                    (short 'f' <> long "force" <> help "Force update")
                    <*> switch
                    (long "fail-if-shadowed" <> help "Fails after upgrading if the upgraded ghcup binary is shadowed by something else in PATH (useful for CI)")
                  )
             <**> helper
             )
             (progDesc "Upgrade ghcup")
           )
      <> command
           "compile"
           (   Compile
           <$> info (compileP <**> helper)
                    (progDesc "Compile a tool from source")
           )
      <> command
           "whereis"
            (info
             (   (Whereis
                     <$> (WhereisOptions <$> switch (short 'd' <> long "directory" <> help "return directory of the binary instead of the binary location"))
                     <*> whereisP
                 ) <**> helper
             )
             (progDesc "Find a tools location"
             <> footerDoc ( Just $ text whereisFooter ))
           )
      <> command
           "prefetch"
            (info
             (   (Prefetch
                     <$> prefetchP
                 ) <**> helper
             )
             (progDesc "Prefetch assets"
             <> footerDoc ( Just $ text prefetchFooter ))
           )
      <> command
           "gc"
            (info
             (   (GC
                     <$> gcP
                 ) <**> helper
             )
             (progDesc "Garbage collection"
             <> footerDoc ( Just $ text gcFooter ))
           )
      <> command
              "run"
               (Run
               <$>
                 info
                   (runOpts <**> helper)
                   (progDesc "Run a command with the given tool in PATH"
                   <> footerDoc ( Just $ text runFooter )
                   )
               )
      <> commandGroup "Main commands:"
      )
    <|> subparser
          (  command
              "debug-info"
              ((\_ -> DInfo) <$> info helper (progDesc "Show debug info"))
          <> command
               "tool-requirements"
               (   ToolRequirements
               <$> info (toolReqP <**> helper)
                        (progDesc "Show the requirements for ghc/cabal")
               )
          <> command
               "changelog"
               (info
                  (fmap ChangeLog changelogP <**> helper)
                  (  progDesc "Find/show changelog"
                  <> footerDoc (Just $ text changeLogFooter)
                  )
               )
          <> command
               "config"
               (   Config
               <$> info (configP <**> helper)
                        (progDesc "Show or set config" <> footerDoc (Just $ text configFooter))
               )
          <> commandGroup "Other commands:"
          <> hidden
          )
    <|> subparser
          (  command
              "install-cabal"
              (info
                 ((InstallCabalLegacy <$> installOpts (Just Cabal)) <**> helper)
                 (  progDesc "Install or update cabal"
                 <> footerDoc (Just $ text installCabalFooter)
                 )
              )
          <> internal
          )
     <|> subparser
          (command
              "nuke"
               (info (pure Nuke <**> helper)
                     (progDesc "Completely remove ghcup from your system"))
           <> commandGroup "Nuclear Commands:"
           <> hidden
          )
     <|> subparser
          (command
              "print-app-errors"
               (info (pure PrintAppErrors <**> helper)
                     (progDesc ""))
           <> internal
          )
