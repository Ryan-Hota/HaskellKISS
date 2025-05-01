module OS_IO (
    mkHardLink
, clearScreenCommand) where

import System.Win32.HardLink (createHardLink)
-- | destination -> source -> action
mkHardLink :: FilePath -> FilePath -> IO ()
mkHardLink = createHardLink

clearScreenCommand :: String
clearScreenCommand = "cls"