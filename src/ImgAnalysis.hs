module ImgAnalysis where

import qualified Data.ByteString.Lazy as BS
import Codec.Picture
import Data.Either
import Data.Map as Map
import Data.List
import qualified Data.Vector.Storable as Vec


type PixelData = (PixelRGB8, Int)

type Cluster = (PixelRGB8, Int, (Int, Int))

analyze :: DynamicImage -> (BS.ByteString, [PixelData])
analyze a = wrapper (convertRGB8 a)

getClustForDebug :: DynamicImage -> String
getClustForDebug a = 
  concat $ fmap (++ "\n") $ fmap show $ fmap (\(_, x, _) -> x) $ getClusters (func(convertRGB8 a))

--returns the modified image for saving, plus an array of pixeldata
--[PixelData] is a list of pairs; each pixel color with its count in the image
wrapper :: Image PixelRGB8 -> (BS.ByteString, [PixelData]) 
wrapper a = createPng (func a) (getData (func a))

createPng :: Image PixelRGB8 -> [PixelData] -> (BS.ByteString, [PixelData])
--createPng a xs = (encodePng a, xs)
createPng a xs = (imgDev a, xs)

imgDev :: Image PixelRGB8 -> BS.ByteString
imgDev a = encodePng( writeClusters a)

writeClusters :: Image PixelRGB8 -> Image PixelRGB8
writeClusters a = outImage (imageWidth a) (imageHeight a) (getClusters a)

outImage :: Int -> Int -> [Cluster] -> Image PixelRGB8
outImage w h cs = generateImage (getClustAt cs) w h

func :: Image PixelRGB8 -> Image PixelRGB8
func a = pixelMap quant a

quant :: PixelRGB8 -> PixelRGB8
quant a = colorMap (\x -> (div x 16) * 17) a

white :: PixelRGB8
white = PixelRGB8 255 255 255

black :: PixelRGB8
black = PixelRGB8 0 0 0

thresh :: PixelRGB8 -> PixelRGB8
thresh (PixelRGB8 a b c) = if (div a 3) + (div b 3) + (div c 3) > 128 then white else black


combineData :: [PixelRGB8] -> Map PixelRGB8 Int -> Map PixelRGB8 Int
combineData ps m = Map.unionsWith (+) [(Map.singleton p 1) | p <- ps]

getData :: Image PixelRGB8 -> [PixelData]
getData a =
  Map.toList $ combineData (getPixels a) Map.empty

getPixels :: Image PixelRGB8 -> [PixelRGB8]
getPixels a = [pixelAt a x y | x <- [0..(imageWidth a) - 1], y <- [0..(imageHeight a) - 1]]


clustP :: Cluster -> PixelRGB8
clustP c = (\(x, _, _) -> x) c

colorClust :: [Cluster] -> PixelRGB8 -> [Cluster]
colorClust xs c = [i | i <-xs, (c == (clustP i))]

genClusters :: Image PixelRGB8 -> [Cluster]
genClusters a = zip3 (getPixels a)
  [1 | x <- [0..(length (getPixels a))]] 
  [(x, y) | x <- [0..(imageWidth a)-1], y <- [0..(imageHeight a)-1]]

getClusters :: Image PixelRGB8 -> [Cluster]
getClusters a = sortBy (\(_,a,_) (_,b,_) -> compare b a ) ( 
  zip3 (nub (getPixels a))
  [length (colorClust (genClusters a) i) | i <- (nub(getPixels a))]
  (getClustPos a (genClusters a)) )

getClustPos :: Image PixelRGB8 -> [Cluster] -> [(Int, Int)]
getClustPos a cs = fmap (getClustSum cs) (nub (getPixels a))

getClustSum :: [Cluster] -> PixelRGB8 -> (Int, Int)
getClustSum as p = 
  let l = [(\(_, _, (x, y)) -> (x, y)) i | i <- as, (clustP i) == p] in
    ((div (sum (fmap fst l)) (length l)), (div (sum (fmap snd l)) (length l)))

clustDiff :: Cluster -> Int -> Int -> Int
clustDiff p x y = 
  (\(_,_,(a,b)) c d -> ((a-c) * (a-c)) + ((b-d) * (b-d))) p x y

clustRad :: Cluster -> Int
clustRad c = (\(_, x, _) -> 1 + (div x 3)) c

getClustAt :: [Cluster] -> Int -> Int -> PixelRGB8
getClustAt cs x y =
  let l = [i | i <- cs, (clustDiff i x y) <= (clustRad i)] in
    if (length l) > 0 then clustP(last l) else clustP (head cs)
