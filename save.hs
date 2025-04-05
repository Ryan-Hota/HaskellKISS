import Data.Foldable (toList)
import Data.List (transpose)
-- | -- | path to .hs file that you want to load, passed as the sole arguement to the executable.
--
-- It's path is assumed to be absolute.

-- exeArg :: IO FilePath
-- exeArg =
--     getArgs
--     >>= safeWhen (length|>(==1))
--     |> fromMaybe ( error "This executable was provided an incorrect number of arguments." )
--     |> safeHead 
--     |> genFilterM doesFileExist 
--     |> fmap ( fromMaybe ( error "The argument provided to the executable is not a path to a file.\nThe executable requires the filepath to the haskell program you mean to load" ) )

    -- do
    -- args <- getArgs
    -- let arg = if length args == 1 then head args else error "This executable was not provided an argument or was provided more than one argument.\nThe executable requires exactly one argument : the filepath to the haskell program you mean to load"
    -- isFile <- doesFileExist arg
    -- let validArg = if isFile then arg else error "The argument provided to the executable is not a path to a file.\nThe executable requires exactly one argument : the filepath to the haskell program you mean to load"
    -- return validArg

data Tree container a = Nil | Node a ( container ( Tree container a ) )

instance Foldable container => Foldable (Tree container) where

    toList :: Tree container a -> [a]
    toList = concat . levels where
        levels Nil = []
        levels (Node node children) = [ node ] : ( concat <$> transpose $ levels <$> toList children )

    foldr :: Foldable container => (a -> b -> b) -> b -> Tree container a -> b
    foldr step base = foldr step base . toList


-- | @\\@ Directory @d ->@ list of ancestor directories of @d@
-- 
-- >>> ancestors "d1\\d2\\d3\\f.x" == ["d1\\d2\\d3\\","d2\\d3\\","d3\\"]
-- ancestors :: FilePath -> [FilePath]
-- ancestors =
--     takeDirectory
--     |> addTrailingPathSeparator
--     |> iterate' ( dropWhile (not.isPathSeparator) |> dropWhile isPathSeparator )
--     |> takeWhile (not.null)

-- | </> for absolute file paths
-- (</>) :: SafePath pathType -> FilePath -> SafePath pathType
-- (</>) (Is pathType path) path' = Is pathType ( path F.</> path' )

-- | @makeRelative@ for absolute file paths
-- mkRelativeFromAbsolute :: AbsoluteFilePath -> AbsoluteFilePath -> FilePath
-- mkRelativeFromAbsolute (Is Absolute path) (Is Absolute path') = F.makeRelative path path'


-- | @doesFileExist@ for absolute file paths
-- doesAbsoluteFileExist :: AbsoluteFilePath -> IO Bool
-- doesAbsoluteFileExist (Is Absolute path) = doesFileExist path


-- | path to the first /options.txt/ file found in the ancestor directories of @target@
-- optionsPath :: Target -> IO (Maybe FilePath)
-- optionsPath =
--     relativeTarget
--     |> ancestors
--     |> map ( </> "options.txt" )
--     |> filterM doesFileExist
--     |> fmap safeHead

-- | Given a target, return the corresponding
--
-- Modules directory in the workspace, \\
-- where the user is supposed to keep all haskell module files\\
-- they want to be available for import everywhere in the workspace.
--
-- The location of the directory is required to be @workspace</>"Modules"@
--
-- return @Nothing@ if such a module isn't found
-- modulesPath :: Target -> IO (Maybe FilePath)
-- modulesPath =
--     workspace
--     |> (</> "Modules")
--     |> Just
--     |> genFilterM doesDirectoryExist