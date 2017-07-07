import Data.List
import Data.Set (fromList, toList)

-- Type aliases
type Money = Int
type CoinBag = [Int]
type Combination = [Int]

countChange :: Money -> CoinBag -> Int
countChange target coins = getResult $ count target coins [[]]
  where
    getResult = length . filter (== target) . fmap sum

count :: Money -> CoinBag -> [Combination] -> [Combination]
count target coins combos
  | processingComplete combos = combos
  | otherwise = count target coins (dedup (combos >>= addCoin))
  where
    addCoin :: Combination -> [Combination]
    addCoin combo
      | sum combo < target = [ sort (coin : combo) | coin <- coins ]
      | otherwise = [combo]
    processingComplete = not . any (<target) . fmap sum

dedup :: Ord a => [a] -> [a]
dedup = toList . fromList
