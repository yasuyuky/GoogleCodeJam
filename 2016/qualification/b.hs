import Control.Applicative
import Control.Monad (forM_)

solve :: String -> Char -> Int -> Int
solve [] l count | l == '+' = count
                 | otherwise = count+1
solve (n:s) l count
  | n == l = solve s l count
  | otherwise = solve s n (count+1)

main = do
  _ <- read <$> getLine :: IO Int
  ss <- lines <$> getContents :: IO [String]
  forM_ (zip [1..] ss) $ \(i,h:s) -> do
    let ans = solve s h 0
    putStrLn $ "Case #"++show i++": "++show ans
