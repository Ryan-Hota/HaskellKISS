module Shadow_IO (
    withShadowOf 
) where

import FilePath (makeAbsolute, AbsoluteFilePath, unWrap, (</>))
import Target_IO (targetPath, Target)
import Utilities ((|>))
import Data.Char (isSpace)
import Data.Functor ((<&>))
import Directory_IO (withCurrentDirectory)
import System.Directory (createDirectoryIfMissing)
import Control.Monad (void)

root :: IO AbsoluteFilePath
root = makeAbsolute "D:" <&> (</>"HaskellKISS") |> (</>"Shadow")

shadowOf :: Target -> IO AbsoluteFilePath
shadowOf = targetPath |> unWrap |> concatMap replace |> pure |> ( (</>) <$> root <*> )
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