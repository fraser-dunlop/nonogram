{-# LANGUAGE OverloadedStrings #-}
module Nonogram.Params where
import Codec.Picture
import Data.Matrix
import Data.Text hiding (transpose, length, words, maximum)
import Data.Semigroup

asBoolMatrix :: Image Pixel8 -> Matrix Bool
asBoolMatrix im = 
  let w = imageWidth im
      h = imageHeight im
  in matrix w h (\(y,x) -> pixelAt im (x-1) (y-1) < 120)


rowHints :: Image Pixel8 -> [[Int]]
rowHints im = hints <$> (toLists $ asBoolMatrix im)
       
colHints :: Image Pixel8 -> [[Int]]
colHints im = hints <$> (toLists $ transpose $ asBoolMatrix im) 

hints :: [Bool] -> [Int]
hints v =
  let str = (\b->if b then 'b' else ' ') <$> v
  in length <$> words str

maxHints :: [[a]] -> Int
maxHints h = maximum $ length <$> h

comment :: Image Pixel8 -> Text
comment im = Data.Text.unlines (("$    " <>) <$> txtLns)
  where strLns = toLists $ (\b -> if b then '#' else '.') <$> asBoolMatrix im 
        txtLns = pack <$> strLns

parameters :: Image Pixel8 -> Text
parameters im = Data.Text.unlines [nRows,nCols,maxRu,rrule,crule]
  where nRows = "letting nRows be " <> (pack $ show $ imageHeight im)
        nCols = "letting nCols be " <> (pack $ show $ imageWidth im)
        maxRu = "letting maxRules be " <> (pack $ show mhint) 
        rrule = "letting rowRules be " <> rrls
        rrls  = replace "],[" "]\n,[" $ pack $ show $ (padZeros mhint <$> rowHints im)
        crule = "letting colRules be " <> crls
        crls  = replace "],[" "]\n,[" $ pack $ show $ (padZeros mhint <$> colHints im)
        mhint = maxHints (colHints im ++ rowHints im)

essParam :: Image Pixel8 -> Text
essParam im = (replace "\n" "<br>" $ Data.Text.unlines ["language ESSENCE 1.5","",comment im,"",parameters im])

padZeros :: Int -> [Int] -> [Int]
padZeros n l = if length l < n
                 then padZeros n $ l ++ [0]
                 else l
