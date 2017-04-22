import Control.Applicative
import Text.Printf
import Debug.Trace


convertTobase_ :: Integer -> String -> Integer -> Integer
convertTobase_ _ [] _ = 0
convertTobase_ a (b:bb) c | b == '1' = a^c + convertTobase_ a bb (c-1)
                          | b == '0' = convertTobase_ a bb (c-1)
                          | otherwise = undefined

convertTobase :: String -> Integer -> Integer
convertTobase b base = convertTobase_ base b  (fromIntegral (length b-1))

makeBinString :: Integer -> String
makeBinString = printf "%b1"

isPsuedoPrime :: Integer -> Bool
isPsuedoPrime n | even n = False
                | otherwise = all (>0) [n`mod`d|d<-take 100 [3,5..r]]
                    where r = (round.sqrt.fromIntegral) n :: Integer

factor1 :: Integer -> Integer
factor1 n | even n = 2
          | otherwise = head [ d|d<-[3,5..r],n`mod`d==0]
              where r = (round.sqrt.fromIntegral) n :: Integer

main = do
  _ <- getLine :: IO String
  [n,j] <- map read.words <$> getLine :: IO [Integer]
  putStr $ unlines $ (:) "Case #1: " $ map unwords $
    take (fromIntegral j) [ bstr:map ((show.factor1).convertTobase bstr) [2..10]
                          | x <- [2^(n-2)..(2^(n-1)-1)]
                          , let bstr = makeBinString x
                          , not.any isPsuedoPrime $ map (convertTobase bstr) [2..10]
                          ]
