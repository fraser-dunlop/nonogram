{-# LANGUAGE ScopedTypeVariables #-}
module Lib
    ( someFunc
    ) where
import Nonogram.PixExp
import Nonogram.Server
import Codec.Picture
import Codec.Picture.Extra
import qualified Data.ByteString as BS (readFile)
import System.Environment (getArgs)
import Text.ParserCombinators.Parsec
import Data.Word
import Data.Char (digitToInt)
import Control.Exception hiding (Handler)

someFunc :: IO ()
someFunc = runServer

downsampleAndSquanch = do
  [impath] <- getArgs
  bs <- BS.readFile impath
  let im = decodeImage bs
  case im of
    Left e -> print e
    Right i -> do
      let rgb8im = convertRGB8 i
          scaled = scaleBilinear 66 90 rgb8im
--          (im8, pal) = palettize (PaletteOptions MedianMeanCut False 3) scaled 
      catch (writePng "test.png" $ pixelMap boolify scaled)
            (\(e :: ExpressionException) -> putStrLn "Caught a custom exception"
                                         >> putStrLn (show e)) 
           


boolify :: PixelRGB8 -> Pixel8 
boolify p = evalPixExp p "r > 120 & g > 120 & b % 120" 

