module GHCup.PlanJson where

import Control.Monad (unless)
import System.FilePath
import System.Directory

findPlanJson
    :: FilePath
    -> IO FilePath
findPlanJson fp = do
    planJsonFn <- do
            mRoot <- findProjectRoot fp
            case mRoot of
                Nothing  -> fail ("missing project root relative to: " ++ fp)
                Just dir -> fromBuilddir $ dir </> "dist-newstyle"

    havePlanJson <- doesFileExist planJsonFn

    unless havePlanJson $
        fail "missing 'plan.json' file; do you need to run 'cabal new-build'?"

    return planJsonFn
  where
    fromBuilddir distFolder = do
        haveDistFolder <- doesDirectoryExist distFolder

        unless haveDistFolder $
            fail ("missing " ++ show distFolder ++ " folder; do you need to run 'cabal new-build'?")

        return $ distFolder </> "cache" </> "plan.json"


-- | Find project root relative to a directory, this emulates cabal's current
-- heuristic, but is slightly more liberal. If no cabal.project is found,
-- cabal-install looks for *.cabal files in the specified directory only. This
-- function also considers *.cabal files in directories higher up in the
-- hierarchy.
findProjectRoot :: FilePath -> IO (Maybe FilePath)
findProjectRoot dir = do
    normalisedPath <- canonicalizePath dir
    let checkCabalProject d = do
            ex <- doesFileExist fn
            return $ if ex then Just d else Nothing
          where
            fn = d </> "cabal.project"

        checkCabal d = do
            files <- listDirectory' d
            return $ if any (isExtensionOf' ".cabal") files
                        then Just d
                        else Nothing

    result <- walkUpFolders checkCabalProject normalisedPath
    case result of
        Just rootDir -> pure $ Just rootDir
        Nothing      -> walkUpFolders checkCabal normalisedPath
  where
    isExtensionOf' :: String -> FilePath -> Bool
    isExtensionOf' ext fp = ext == takeExtension fp

    listDirectory' :: FilePath -> IO [FilePath]
    listDirectory' fp = filter isSpecialDir <$> getDirectoryContents fp
      where
        isSpecialDir f = f /= "." && f /= ".."

walkUpFolders :: (FilePath -> IO (Maybe a)) -> FilePath -> IO (Maybe a)
walkUpFolders dtest d0 = do
    home <- getHomeDirectory

    let go d | d == home  = pure Nothing
             | isDrive d  = pure Nothing
             | otherwise  = do
                   t <- dtest d
                   case t of
                     Nothing  -> go $ takeDirectory d
                     x@Just{} -> pure x

    go d0

