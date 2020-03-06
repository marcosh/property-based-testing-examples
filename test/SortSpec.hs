{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SortSpec where

import Data.Foldable (toList)
import Data.List (delete, sort)

-- extra
import Data.List.Extra (snoc)

-- Fin
import Data.Fin as Fin
import Data.Fin.List hiding (swap)
import Data.Fin.Permutation

-- hspec
import Test.Hspec

-- peano
import Data.Peano as Peano

-- QuickCheck
import Test.QuickCheck

-- safe
import Safe (minimumMay, maximumMay)

-- see https://twitter.com/BeRewt/status/1235845424218435586
preserves :: Eq a => [a] -> [a] -> Bool
preserves [] [] = True
preserves (x : xs) ys = x `elem` ys && preserves xs (delete x ys)
preserves _ _ = False

ordered :: Ord a => [a] -> Bool
ordered [] = True
ordered [_] = True
ordered (x : y : xs) = x <= y && ordered (y : xs)

instance Arbitrary a => Arbitrary (List ('Peano.Succ ('Peano.Succ ('Peano.Succ ('Peano.Succ 'Peano.Zero)))) a) where
  arbitrary = (:.) <$> arbitrary <*> ((:.) <$> arbitrary <*> ((:.) <$> arbitrary <*> ((:.) <$> arbitrary <*> pure Nil)))
  shrink = (: [])

instance Arbitrary (Fin.Fin ('Peano.Succ ('Peano.Succ ('Peano.Succ ('Peano.Succ 'Peano.Zero))))) where
  arbitrary = oneof [pure Fin.Zero, pure $ Fin.Succ Fin.Zero, pure $ Fin.Succ (Fin.Succ Fin.Zero)]
  shrink = (: [])

instance Arbitrary (Permutation ('Peano.Succ ('Peano.Succ ('Peano.Succ ('Peano.Succ 'Peano.Zero))))) where
  arbitrary = mconcat <$> listOf (swap <$> arbitrary <*> arbitrary)
  shrink = (: [])

spec :: Spec
spec = do
  describe "sort" $ do
    -- this is beaten by id
    it "preserves the length of the input" $ property $
      \(x :: String) -> (length x == length (sort x))

    -- this is beaten by (const [])
    it "is idempotent" $ property $
      \(x :: String) -> sort x == sort (sort x)

    -- this is beaten by (const [])
    it "produces an ordered list" $ property $
      \(x :: String) -> ordered (sort x)

    -- this is beaten by id
    it "preserves the element in the input" $ property $
      \(x :: String) -> preserves x (sort x)

    -- this is beaten by id
    it "does not produce new elements" $ property $
      \(x :: String) -> preserves (sort x) x

    it "puts the maximum at the end" $ property $
      \(x :: String) -> let xmax = maximumMay x
                        in  (snoc (sort x) <$> xmax) == (sort . (: x) <$> xmax)

    it "puts the minimum at the beginning" $ property $
      \(x :: String) -> let xmin = minimumMay x
                        in  (flip (:) (sort x) <$> xmin) == (sort . snoc x <$> xmin)

    -- this is beaten by (const [])
    it "produces the same result for any permutation of the input" $ property $
      \( x :: List ('Peano.Succ ('Peano.Succ ('Peano.Succ ('Peano.Succ 'Peano.Zero)))) Char
       , p :: Permutation ('Peano.Succ ('Peano.Succ ('Peano.Succ ('Peano.Succ 'Peano.Zero))))
       ) -> sort (toList x) == (sort $ toList (apply p x))
