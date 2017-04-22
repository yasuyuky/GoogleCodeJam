import Control.Monad
import Text.Printf

main = do
  _ <- getLine :: IO String
  kcss <- map (map read.words).lines <$> getContents :: IO [[Integer]]
  forM_ (zip [1..] kcss) $ \(i,[k,c,s]) -> do
    putStr ("Case #"++show i++": ")
    putStrLn $ case (k,c,s) of
      _ | k == 1 -> "1"
        | c == 1 && k==s -> unwords [show x|x<-[1..k]]
        | k <= c -> show ((k^k-k)`div`(k-1)^2)
        | k == c+1 && s >= 2 -> unwords $ map show [(k^c-1)`div`c^2,k^c-(k^c-1)`div`c^2+1]
        | otherwise ->  "IMPOSSIBLE"
