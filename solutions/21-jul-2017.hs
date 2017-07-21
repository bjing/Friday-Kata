module Combinations where
import Data.List
import Control.Monad

combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs
                           , ys <- combinations (n-1) xs']


-- Another more elegant solution
combinations' :: Int -> [a] -> [[a]]
combinations' n = filter ((==n) . length) . sublists

sublists :: [a] -> [[a]]
sublists = filterM (const [True, False])
