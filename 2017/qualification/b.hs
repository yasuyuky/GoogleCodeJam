import Control.Arrow ((&&&))
import Control.Monad (forM_)
import Data.List (group)

getTidy :: [(Int,Int)] -> String
getTidy [] = ""
getTidy [(x,l)] = concat(replicate l (show x))
getTidy ((x,lx):ns@((y,_):_))
  | x <= y = concat(replicate lx (show x))++getTidy ns
  | otherwise = case x of
      1 -> concat(replicate (lx-1) "9")++concatMap (\(_,l)->replicate l '9') ns
      _ -> show (x-1)++concat(replicate (lx-1) "9")++concatMap (\(_,l)->replicate l '9') ns

main = do
  _ <- read <$> getLine :: IO Int
  ss <- lines <$> getContents :: IO [String]
  forM_ (zip [1..] ss) $ \(i,s) -> do
    let gs = map (read.(:[]).head&&&length) $ group s :: [(Int,Int)]
    putStrLn $ "Case #"++show i++": "++getTidy gs
