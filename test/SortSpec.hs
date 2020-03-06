{-# LANGUAGE ScopedTypeVariables #-}

module SortSpec where

import Data.List (delete, sort)

-- extra
import Data.List.Extra (snoc)

-- hspec
import Test.Hspec

-- QuickCheck
import Test.QuickCheck

-- safe
import Safe (minimumMay, maximumMay)

-- see https://twitter.com/BeRewt/status/1235845424218435586
preserves :: Eq a => [a] -> [a] -> Bool
preserves [] [] = True
preserves (x : xs) ys = x `elem` ys && preserves xs (delete x ys)

ordered :: Ord a => [a] -> Bool
ordered [] = True
ordered [x] = True
ordered (x : y : xs) = x <= y && ordered (y : xs)

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
