import Control.Applicative
import qualified Data.Set as Set

solve :: Set.Set Char -> Integer -> Integer -> String
solve _ _ 0 = "INSOMNIA"
solve track count n =
  let nstr = show (n*count)
      track' = Set.union track (Set.fromList nstr)
  in if Set.size track' == 10 then nstr else solve track' (count+1) n

main = do
  _ <- getLine :: IO String
  ns <- map read.lines <$> getContents :: IO [Integer]
  putStr $ unlines $ zipWith (\i a->"Case #"++show i++": "++a) [1..] $ map (solve Set.empty 1) ns
