module ImgAnalysis where

import qualified Data.ByteString.Lazy as BS
import Codec.Picture
import Data.Either
import Data.Map as Map
import Data.List
import qualified Data.Vector.Storable as Vec


--type PixelData = (PixelRGB8, Int)

type Cluster = (PixelRGB8, Int, (Int, Int))

analyze :: DynamicImage -> (BS.ByteString, [Cluster])
analyze a = let n = func (convertRGB8 a) in
  (encodePng(writeClusters n (getClusters n)), getClusters n)

writeClusters :: Image PixelRGB8 -> [Cluster] -> Image PixelRGB8
writeClusters a b = outImage (imageWidth a) (imageHeight a) b

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
thresh (PixelRGB8 a b c) = if (div a 2) + (div b 3) + (div c 4) > 128 then white else black

--this just counts how many pixels of each color are in the image.
--this data is also collected when we build clusters, but it was easier to
--collect it again than to extract it from the cluster
combineData :: [PixelRGB8] -> Map PixelRGB8 Int -> Map PixelRGB8 Int
combineData ps m = Map.unionsWith (+) [(Map.singleton p 1) | p <- ps]

getPixels :: Image PixelRGB8 -> [PixelRGB8]
getPixels a = [pixelAt a x y | x <- [0..(imageWidth a) - 1], y <- [0..(imageHeight a) - 1]]

--given a cluster, returns its color
clustP :: Cluster -> PixelRGB8
clustP c = (\(x, _, _) -> x) c

--given a list of clusters and a color, returns all clusters with that color
colorClust :: [Cluster] -> PixelRGB8 -> [Cluster]
colorClust xs c = [i | i <-xs, (c == (clustP i))]

--given an image, makes a naive list: all pixels are in a cluster of size 1,
--and the center of the cluster is just the location of the pixel. The 
--getClusters function makes this into something more useful
genClusters :: Image PixelRGB8 -> [Cluster]
genClusters a = zip3 (getPixels a)
  [1 | x <- [0..(length (getPixels a))]] 
  [(x, y) | x <- [0..(imageWidth a)-1], y <- [0..(imageHeight a)-1]]

--given our image, returns a list of clusters sorted by size (smallest first, so
--that small clusters aren't always obscured by big ones
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
