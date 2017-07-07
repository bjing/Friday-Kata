import Data.List
import Data.Set (fromList, toList)

-- Type aliases
type Money = Int
type CoinBag = [Int]
type Combination = [Int]

countChange :: Money -> CoinBag -> Int
countChange target coins = getResult target $ count [[]] target coins

count :: [Combination] -> Money -> CoinBag -> [Combination]
count combos target coins
  | processingComplete target combos = combos
  | otherwise = count (dedup (combos >>= addCoin target coins)) target coins

addCoin :: Money -> CoinBag -> Combination -> [Combination]
addCoin target coins combo
  | sum combo < target = [ sort (coin : combo) | coin <- coins ]
  | otherwise = [combo]

-- Utilities
processingComplete :: Money -> [Combination] -> Bool
processingComplete target = not . any (<target) . fmap sum

getResult :: Money -> [Combination] -> Int
getResult target = length . filter (== target) . fmap sum

dedup :: Ord a => [a] -> [a]
dedup = toList . fromList
