module Directory_IO (
    fileTreeAlong,
    doesFileExist,
    mkLinkAt,
    fileTreeUnder
) where

import FilePath (
    (</>),
    splitPath,
    unWrap,
    AssuredToBe, Absolutable (..), takeName
    )
    
import Utilities ((|>), ifThenElse, (||>))
import System.IO.Unsafe (unsafeInterleaveIO)
import Directory (FileTree, FileTree (..))
import System.FilePath (equalFilePath)
import qualified System.Directory as D
import IO (readPermittedFile)
import Link_IO (mkHardLink)

-- | @\\ dir file ->@\\
-- make a  __/hard/__  link located in @dir@ that points to the @file@\\
-- and\\
-- return the name of the created link
mkLinkAt :: (Absolutable pathType0, Absolutable pathType1) => AssuredToBe pathType0 -> AssuredToBe pathType1 -> IO String
mkLinkAt dir file = 
    ifThenElse
    <$> doesFileExist (dir</>name) 
    <*> pure name
    <*> unsafeInterleaveIO ( 
        mkHardLink (unWrap (toAbsolute file)) (unWrap (toAbsolute (dir</>name)))
        >> pure name
    )
    where name = takeName file

doesFileExist :: Absolutable pathType => AssuredToBe pathType -> IO Bool
doesFileExist = toAbsolute |> unWrap |> D.doesFileExist

listPermittedDirectory :: Absolutable pathType => AssuredToBe pathType -> IO [String]
listPermittedDirectory path = -- pure $ error "check lazy"
    ifThenElse 
        <$> ( path ||> unWrap |> D.getPermissions |> fmap D.searchable )
        <*> ( path ||> toAbsolute |> unWrap |> D.listDirectory )
        <*> pure []

-- | Given /target/, get the file tree that surrounds the path to /target/
fileTreeAlong :: Absolutable pathType => AssuredToBe pathType -> IO (FileTree (AssuredToBe pathType))
fileTreeAlong =
    splitPath
    |> ( \ ( topDir , descendants ) -> fileTreeAlongNamed ( unWrap topDir ) topDir descendants )

fileTreeAlongNamed :: Absolutable pathType => String -> AssuredToBe pathType -> [String] -> IO (FileTree (AssuredToBe pathType))
fileTreeAlongNamed name_ path (child:childDescendants) =
    ( listDir <$> fileTreeUnderNamed name_ path )
    ||> fmap ( filter (name|>(not.equalFilePath child)) )
    ||> unsafeInterleaveIO    
    ||> ( (:) <$> fileTreeAlongNamed child ( path </> child ) childDescendants <*> )
    ||> fmap ( Directory name_ path )
fileTreeAlongNamed name path [] = fileTreeUnderNamed name path

fileTreeUnder :: Absolutable pathType => AssuredToBe pathType -> IO (FileTree (AssuredToBe pathType))
fileTreeUnder = fileTreeUnderNamed <$> takeName <*> id

fileTreeUnderNamed :: Absolutable pathType => String -> AssuredToBe pathType -> IO (FileTree (AssuredToBe pathType))
fileTreeUnderNamed name path =
    ifThenElse
        <$> doesFileExist path
        <*> (
            readPermittedFile path
            ||> unsafeInterleaveIO
            ||> fmap ( File name path )
        )
        <*> (
            listPermittedDirectory path
            >>= mapM ( fileTreeUnderNamed <$> id <*> ( path </> ) )
            ||> unsafeInterleaveIO
            ||> fmap ( Directory name path  )
        )
    ||> unsafeInterleaveIO
