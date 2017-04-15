import Control.Arrow ((&&&))

flipChars s 0 = s
flipChars (c:s) k
  | c == '+' = '-':flipChars s (k-1)
  | c == '-' = '+':flipChars s (k-1)
  | otherwise = undefined

solve n ss@(c:s) k
  | length ss < k = if all (=='+') ss
                      then show n
                      else "IMPOSSIBLE"
  | length ss >= k = case c of
                       '+' -> solve n s k
                       '-' -> solve (n+1) (flipChars s (k-1)) k
                       _ -> undefined

main = do
  _ <- getLine :: IO String
  ls <- map ((head&&&(read.head.tail)).words).lines <$> getContents :: IO [(String,Int)]
  putStr $ unlines $ zipWith (\i a->"Case #"++show i++": "++a) [1..] $ map (uncurry(solve 0)) ls
