module IO (
    readPermittedFile
) where
import FilePath (Absolutable (..), AssuredToBe, unWrap)
import Utilities (ifThenElse, (||>), (|>))
import System.Directory (getPermissions, Permissions (..))

readPermittedFile :: Absolutable pathType => AssuredToBe pathType -> IO String
readPermittedFile path =
    ifThenElse
        <$> ( path ||> unWrap |> getPermissions |> fmap readable )
        <*> ( path ||> toAbsolute |> unWrap |> readFile )
        <*> pure "read permission denied"