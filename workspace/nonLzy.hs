import System.Directory (doesFileExist, getDirectoryContents)

ifThenElse :: Bool -> p -> p -> p
ifThenElse True  = \ x _ -> x
ifThenElse False = \ _ y -> y

-- |
-- if path is a file,
--    then read it
-- if path is a directory, 
--    then show a list of the entities in the directory
showContents :: FilePath -> IO String
showContents path = 
    ifThenElse 
        <$> doesFileExist path
        <*> readFile path
        <*> ( show <$> getDirectoryContents path )

main :: IO String
main = 
    writeFile "myFile.txt" "Hello World!"
    >> showContents "myFile.txt"