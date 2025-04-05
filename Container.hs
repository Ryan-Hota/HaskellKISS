module Container where

import Prelude hiding (map,filter)
import qualified Control.Applicative as A
import Utilities ( (|>), ifThenElse )
import Control.Monad (mfilter)

class (Traversable c, A.Alternative c) => Container c
    
--_~=formation

-- | > { }
empty :: Container c => c x
empty = A.empty

-- | > \ x -> { x }
mkSingleton :: Container c => x -> c x
mkSingleton = pure

-- | > \ x c -> {x} `union` c
insert :: Container c => x -> c x -> c x
insert = mkSingleton |> union

-- | > \ x c -> c \ {x}
delete :: (Eq x,Container c) => x -> c x -> c x
delete = ( mkSingleton :: x -> [x] ) |> flip difference

--_detection

-- | > == { }
isEmpty :: Container c => c x -> Bool
isEmpty = foldr ( \ _ _ -> True ) False

-- | -- | > == { }
isNonEmpty :: Container c => c x -> Bool
isNonEmpty = isEmpty |> not

-- | find the first element which satisfies a condition,\\
-- return Nothing if such an element is not found
find :: Container c => (x -> Bool) -> c x -> Maybe x
find = filter |> ( |> transfer )

-- | > \ x c -> Is x in c ?
isIn :: (Eq x, Container c) => x -> c x -> Bool
isIn = (==) |> filter |> ( |> isNonEmpty )

-- | > \ c x -> does c contain x ?
contains :: (Eq x, Container c) => c x -> x -> Bool
contains c x =  x `isIn` c

--_operations

-- | > \ c1 c2 -> { x | x isIn c1 or x isIn c2 }
union :: Container c => c x -> c x -> c x
union = (A.<|>)

-- | > \ C -> { x |  x isIn c , c isIn C }
unionC :: (Container c1, Container c2) => c1 (c2 x) -> c2 x
unionC = foldr union empty

-- | > \ c1 c2 -> { x | x isIn c1 and x isIn c2 }
intersection :: (Eq x, Container c1, Container c2) => c1 x -> c2 x -> c2 x 
intersection = contains |> filter

-- | > \ C -> { x | forall c in C , x isIn c }
-- WARNING -- throws error at runtime if C isEmpty
intersectionC ::(Eq x, Container c1, Container c2) => c1 (c2 x) -> c2 x 
intersectionC = foldr1 intersection

-- | > \ c1 c2 -> c1 - c2
difference :: (Eq x, Container c1, Container c2) => c1 x -> c2 x -> c1 x
difference = flip ( contains |> ( |> not ) |> filter )

-- | > \ c1 c2 -> { (x,y) | x isIn c1 , y isIn c2 }
product :: Container c => c x -> c y -> c (x,y)
product = A.liftA2 (,)

-- | > \ { c1, c2, c3, ... } -> { [ x1, x2, x3, ... ] | xi isIn ci }
productC :: (Eq x, Container c1, Container c2) => c1 (c2 x) -> c2 [x] 
productC = foldr (A.liftA2 (:)) (mkSingleton [])

--_transformations

-- | > \ c -> { f x | x isIn c }
map :: Container c => (x -> y) -> c x -> c y
map = fmap

-- | take the elements of one container, and move it to another
transfer :: (Container c1, Container c2) => c1 x -> c2 x
transfer = alterBy id
-- transfer = map mkSingleton |> unionC 

-- | > \ predicate c -> { x | x isIn c , x satisfies predicate }
filter :: Container c => (x -> Bool) -> c x -> c x
filter = mfilter |> alterBy
-- filter predicate =
--     map ( \ x ->
--         if predicate x
--         then mkSingleton x
--         else empty
--     )
--     |> unionC

-- | @
-- alterBy f c1 c2 =
--        map Just c1
--    ||> insert Nothing
--    ||> map f
--    ||> filter (/=Nothing)
--    ||> map ( \\ (Just x) -> x )
--    ||> transfer
-- @
--
-- Note : all other transformations can be defined in terms of alterBy
alterBy :: (Container c1, Container c2) => (Maybe x -> Maybe x) -> c1 x -> c2 x
alterBy alteration =
    map Just
    |> flip union (mkSingleton Nothing)
    |> map (
        alteration -- Maybe x -> Maybe x
        |> map mkSingleton -- Maybe x -> Maybe (c2 x)
        |> unionC -- Maybe (c2 x) -> c2 x
    )
    |> unionC

--_Kleisli

-- | filterM, generaliized to any container, not just []
filterM :: (Container c, Applicative f) => (x -> f Bool) -> c x -> f (c x)
filterM predicate =
    traverse ( \ x ->
        ifThenElse
            <$> predicate x
            <*> pure ( mkSingleton x )
            <*> pure empty
    )
    |> fmap unionC

--_instances

instance Container []
instance Container Maybe