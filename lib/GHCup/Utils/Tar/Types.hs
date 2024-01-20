{-# LANGUAGE CPP               #-}
#if defined(TAR)
{-# LANGUAGE DeriveGeneric     #-}
#endif

module GHCup.Utils.Tar.Types
  ( ArchiveResult(..)
  )
  where

#if defined(TAR)

import           Control.Exception              ( Exception )
import           Control.DeepSeq                ( NFData )
import qualified GHC.Generics                   as GHC

data ArchiveResult = ArchiveFatal
                   | ArchiveFailed
                   | ArchiveWarn
                   | ArchiveRetry
                   | ArchiveOk
                   | ArchiveEOF
  deriving (Eq, Show, GHC.Generic)

instance NFData ArchiveResult

instance Exception ArchiveResult

#else

import           Codec.Archive                  ( ArchiveResult(..) )

#endif
