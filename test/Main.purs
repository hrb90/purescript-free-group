module Test.Main where

import Prelude

import Control.Monad.Eff.Console (log)
import Data.Group (class Group, ginverse)
import Data.Group.Free (Free(..), Signed(..))
import Data.List (List)
import Data.Monoid (class Monoid, (<>), mempty)
import Data.Semigroup (class Semigroup)
import Data.Tuple (Tuple(..))
import Test.QuickCheck (class Arbitrary, QC, arbitrary, quickCheck')
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Laws (A, checkLaws)
import Test.QuickCheck.Laws.Data.Monoid (checkMonoid)
import Test.QuickCheck.Laws.Data.Semigroup (checkSemigroup)
import Type.Proxy (Proxy(..))

checkGroup :: forall eff g
              . Group g
              => Arbitrary g
              => Eq g
              => Proxy g
              -> QC eff Unit
checkGroup _ = do
  log "Checking left cancellation law for Group"
  quickCheck' 1000 leftCancel
  log "Checking right cancellation law for Group"  
  quickCheck' 1000 rightCancel

  where

  leftCancel :: g -> Boolean
  leftCancel x = (ginverse x) <> x == mempty
  
  rightCancel :: g -> Boolean
  rightCancel x = x <> (ginverse x) == mempty

newtype F a = F (Free a)

instance arbitraryF :: Arbitrary a => Arbitrary (F a) where
  arbitrary = mapArb <$> arbitrary' where
    arbitrary' :: Gen (List (Tuple a Boolean))
    arbitrary' = arbitrary

    mapSigned (Tuple x true)  = Positive x
    mapSigned (Tuple x false) = Negative x

    mapArb = F <<< Free <<< (map mapSigned)

derive instance eqF :: Eq a => Eq (F a)

instance semigroupF :: Eq a => Semigroup (F a) where
  append (F x) (F y) = F $ append x y

instance monoidF :: Eq a => Monoid (F a) where
  mempty = F $ mempty

instance groupF :: Eq a => Group (F a) where
  ginverse (F x) = F $ ginverse x

main :: forall eff. QC eff Unit
main = checkLaws "Free group" do
  checkSemigroup prxFree
  checkMonoid prxFree
  checkGroup prxFree
  where
  prxFree = Proxy :: Proxy (F A)