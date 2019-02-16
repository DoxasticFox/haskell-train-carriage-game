module Util
( decTo
, fastMap
, split
) where

import           Control.Parallel.Strategies
import qualified Data.Map.Lazy as M

decTo
    :: Integer
    -> Integer
    -> Integer
    -> [Integer]
decTo base n width =
    reverse $ decTo' n width
    where
        decTo' n width | n <= 0    = replicate (fromIntegral width) 0
                       | otherwise = n `rem` base : decTo' (n `quot` base) (width - 1)

split
    :: [Integer]
    -> Integer
    -> ([Integer], [Integer])
split xs n =
    let
        mask  = decTo 2 n (toInteger $ length xs)
        left  = [x | (x, m) <- zip xs mask, m == 1]
        right = [x | (x, m) <- zip xs mask, m /= 1]
    in
        (left, right)

-- | Behaves like `map` but incorporates memoisation and parallelisation.
-- | The performance improvement over `map` is a little disappointing, but it's
-- | still better than nothing. It's possible that if someone who knew what they
-- | were doing took a shot at writing this, their version would take half the
-- | time to run.
fastMap
    :: Ord a
    => (a -> b)
    -> [a]
    -> [b]
fastMap f xs = runEval $ fastMap' M.empty f xs
    where
        fastMap'
            :: Ord a
            => M.Map a b
            -> (a -> b)
            -> [a]
            -> Eval [b]
        fastMap' cache f [] = return []
        fastMap' cache f (x:xs) = do
            v <- rpar $ M.findWithDefault (f x) x cache
            vs <- fastMap' (M.insert x v cache) f xs
            return $ v:vs
