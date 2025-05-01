import Lib

main :: IO ( Maybe Int )
main = return $ search ( == 1 ) [ 2 , 1 ]