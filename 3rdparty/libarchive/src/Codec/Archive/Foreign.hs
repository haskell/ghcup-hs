-- | Import this module to get access to the C API.
--
-- Functions that
-- are deprecated in the C API are not exposed.
module Codec.Archive.Foreign ( module Codec.Archive.Foreign.ArchiveEntry
                             , module Codec.Archive.Foreign.Archive
                             ) where

import           Codec.Archive.Foreign.Archive
import           Codec.Archive.Foreign.ArchiveEntry
