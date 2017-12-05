module Data.Group.Free where
  
import Prelude

import Data.Foldable (foldr)
import Data.Group (class Group)
import Data.List (List(..), reverse, (:))
import Data.Monoid (class Monoid)

data Signed a = Positive a | Negative a

instance showSigned :: Show a => Show (Signed a) where
  show (Positive x) = "+" <> show x
  show (Negative x) = "-" <> show x

type FG a = List (Signed a)

canonical :: forall a. Eq a => FG a -> FG a
canonical = foldr cancelOrPush Nil where
  cancelOrPush x@(Positive x1) y@(Cons (Negative y1) tl) = if x1 == y1 then tl else x : y
  cancelOrPush x@(Negative x1) y@(Cons (Positive y1) tl) = if x1 == y1 then tl else x : y
  cancelOrPush x y = x : y

newtype FreeGroup a = FreeGroup (FG a)

instance showFreeGrp :: Show a => Show (FreeGroup a) where
  show (FreeGroup x) = show x

instance semigrpFreeGrp :: Eq a => Semigroup (FreeGroup a) where 
  append (FreeGroup x) (FreeGroup y) = FreeGroup $ canonical $ x <> y

instance monoidFreeGrp :: Eq a => Monoid (FreeGroup a) where
  mempty = FreeGroup Nil

instance groupFreeGrp :: Eq a => Group (FreeGroup a) where
  ginverse (FreeGroup fg) = FreeGroup $ reverse $ map flipSign fg where
    flipSign (Positive x) = Negative x
    flipSign (Negative x) = Positive x