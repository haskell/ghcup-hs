{-# LANGUAGE TemplateHaskell  #-}

module GHCup.System.Process.Common where


import           GHC.IO.Exception
import           Optics                  hiding ((<|), (|>))
import           Text.PrettyPrint.HughesPJClass hiding ( (<>) )

import qualified Data.ByteString.Lazy          as BL



data ProcessError = NonZeroExit Int FilePath [String]
                  | PTerminated FilePath [String]
                  | PStopped FilePath [String]
                  | NoSuchPid FilePath [String]
                  deriving Show

instance Pretty ProcessError where
  pPrint (NonZeroExit e exe args) =
    text "Process" <+> pPrint exe <+> text "with arguments" <+> pPrint args <+> text "failed with exit code" <+> text (show e <> ".")
  pPrint (PTerminated exe args) =
    text "Process" <+> pPrint exe <+> text "with arguments" <+> pPrint args <+> text "terminated."
  pPrint (PStopped exe args) =
    text "Process" <+> pPrint exe <+> text "with arguments" <+> pPrint args <+> text "stopped."
  pPrint (NoSuchPid exe args) =
    text "Could not find PID for process running " <+> pPrint exe <+> text " with arguments " <+> text (show args) <+> text "."

data CapturedProcess = CapturedProcess
  { _exitCode :: ExitCode
  , _stdOut   :: BL.ByteString
  , _stdErr   :: BL.ByteString
  }
  deriving (Eq, Show)

makeLenses ''CapturedProcess
