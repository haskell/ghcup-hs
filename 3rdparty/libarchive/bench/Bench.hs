module Main (main) where

import           Codec.Archive
import           Codec.Archive.Tar      (Entries (..), FormatError)
import qualified Codec.Archive.Tar      as Tar
import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
import           Criterion.Main
import qualified Data.ByteString.Lazy   as BSL
import           Data.Conduit.Tar       as TarConduit
import           System.IO.Temp         (withSystemTempDirectory)

roundtrip :: BSL.ByteString -> Either ArchiveResult BSL.ByteString
roundtrip = fmap entriesToBSL . readArchiveBSL

failTar :: Entries a -> Either a [Tar.Entry]
failTar (Next e es) = (e :) <$> failTar es
failTar Done        = Right []
failTar (Fail e)    = Left e

roundtripTar :: BSL.ByteString -> Either FormatError BSL.ByteString
roundtripTar = fmap Tar.write . failTar . Tar.read

unpack :: IO (Either ArchiveResult ())
unpack = withSystemTempDirectory "libarchive" $
    \fp -> runArchiveM $ unpackArchive "test/data/libarchive-1.0.5.1.tar" fp

unpackHs :: IO (Either ArchiveResult ())
unpackHs = withSystemTempDirectory "libarchive" $
    \fp -> runArchiveM $ unpackToDirLazy fp =<< liftIO (BSL.readFile "test/data/libarchive-1.0.5.1.tar")

extractTar :: IO ()
extractTar = withSystemTempDirectory "tar" $
    \fp -> Tar.extract fp "test/data/libarchive-1.0.5.1.tar"

-- I'm not even sure why I'm benchmarking this since it doesn't work
unpackTarConduit :: IO ()
unpackTarConduit = withSystemTempDirectory "tar" $
    \fp -> void $ TarConduit.extractTarballLenient "test/data/libarchive-1.0.5.1.tar" (Just fp)

main :: IO ()
main =
    defaultMain [ env file $ \ f ->
                  bgroup "roundtrip"
                      [ bench "libarchive" $ nf roundtrip f
                      , bench "tar" $ nf roundtripTar f
                      ]
                , bgroup "unpack"
                      [ bench "libarchive (via bytestring)" $ nfIO unpackHs
                      , bench "libarchive (C API)" $ nfIO unpack
                      , bench "tar" $ nfIO extractTar
                      , bench "tarConduit" $ nfIO unpackTarConduit
                      ]
                ]
    where file = BSL.readFile "test/data/libarchive-1.0.5.1.tar"
