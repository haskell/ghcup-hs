module Codec.Archive.Test ( simpleFile
                          , simpleDir
                          , stripOwnership
                          , stripTime
                          ) where

import           Codec.Archive

simpleFile :: FilePath -> EntryContent -> Entry
simpleFile name what = Entry name what standardPermissions (Ownership (Just "root") (Just "root") 0 0) (Just (0, 0))

simpleDir :: FilePath -> Entry
simpleDir name = Entry name Directory dirPermissions (Ownership (Just "root") (Just "root") 0 0) (Just (0, 0))

dirPermissions :: Permissions
dirPermissions = executablePermissions

stripOwnership, stripTime :: Entry -> Entry
stripOwnership entry = entry { ownership = Ownership Nothing Nothing 0 0 }
stripTime entry = entry { time = Nothing }
