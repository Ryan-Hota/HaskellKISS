{-# OPTIONS_GHC -Wno-missing-fields #-}
module Options (
    options
) where

import Utilities ((|>), (||>))
import FilePath (RootRelativeFilePath,(</>))
import Directory (FileTree(..), isFile, isDirectory)
import Data.List (iterate', find)
import Data.Maybe (fromMaybe)

-- | characters to ignore if found in the /options.txt/ file
charsToIgnore :: String
charsToIgnore = "[]{}(),;:"

-- | format a text string containing all options into a list of options
format :: String -> [String]
format =
    map ( \ c -> if c `elem` charsToIgnore then ' ' else c )
    |> words

-- | list of options
options :: FileTree RootRelativeFilePath -> [String]
options tree =
    tree
    ||> iterate' (listDir|>head)
    |> takeWhile isDirectory
    |> reverse
    |> concatMap listDir
    |> find ((&&)<$>isFile<*>name|>(=="options.txt"))
    |> fromMaybe defaultOptionsFile
    |> (format.contents)
    where
        defaultOptionsFile =
            File {
                name = "options.txt",
                path = path tree</>"options.txt",
                contents = ""
            }
