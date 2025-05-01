module Lib (search) where

import Data.List (find, transpose)

data Tree x = EmptyTree | Leaf x | Branch ( Tree x ) ( Tree x )

instance Semigroup ( Tree x ) where  (<>)  =  Branch   -- not associative !
instance  Monoid   ( Tree x ) where mempty = EmptyTree

bfs :: Tree x -> [ x ]
bfs = concat . levels where
    levels EmptyTree = [ ]
    levels ( Leaf x ) = [ [ x ] ]
    levels ( Branch tl tr ) = [ ] : ( concat <$> transpose $ levels <$> [ tl , tr ] )

search :: Foldable l => ( x -> Bool ) -> l x -> Maybe x
search predicate = find predicate . bfs . foldMap Leaf