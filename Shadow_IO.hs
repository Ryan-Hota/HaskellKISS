module Shadow_IO (
    withShadowOf 
) where

import FilePath (makeAbsolute, AbsoluteFilePath, unWrap, (</>))
import Target_IO (targetPath, Target)
import Utilities ((|>), (||>))
import Data.Char (isSpace)
import Data.Functor ((<&>))
import Directory_IO (withCurrentDirectory)
import System.Directory (createDirectoryIfMissing )
import System.FilePath (pathSeparator, takeDrive)

root :: Target -> IO AbsoluteFilePath
root target = makeAbsolute ( takeDrive $ unWrap $ targetPath target ) <&> (</>"HaskellKISS") |> (</>"Shadow")

shadowOf :: Target -> IO AbsoluteFilePath
shadowOf target = 
    target 
    ||> targetPath 
    |> unWrap 
    |> concatMap replace 
    |> pure 
    |> ( (</>) <$> root target <*> )
    where
        replace               char
            | isSpace         char = "_SPACE_"
            | ':' ==          char = "_COLON_"
            | '.' ==          char = "_DOT_"
            | otherwise            = [char]

withShadowOf :: Target -> IO r -> IO ( AbsoluteFilePath , r )
withShadowOf trgt action = do 
    shadowDir <- shadowOf trgt
    createDirectoryIfMissing True $ unWrap shadowDir
    returnVal <- withCurrentDirectory shadowDir action
    return ( shadowDir , returnVal )