
module Stl.Write (Vertex, Facet, Mesh, translate, up, putStl, renderStl) where

import Linear.V3
import Text.Printf
import Data.List
import Data.Foldable

import qualified Data.ByteString.Lazy as B
import Data.Binary.Put

type Vertex = V3 Double
type Facet  = V3 Vertex
type Mesh   = [Facet]

putStl :: Mesh -> B.ByteString
putStl mesh = runPut $ header <> len <> foldMap putFacet mesh
    where
        header = foldMap putWord8 (replicate 80 0)
        len    = putInt32le $ fromIntegral $ length mesh

putFacet :: Facet -> Put
putFacet f = putVertex (normal f) <> foldMap putVertex f <> putInt16le 0

putVertex :: Vertex -> Put
putVertex = foldMap (putFloatle . realToFrac)

renderStl :: Mesh -> String
renderStl fs = "solid \n" ++ intercalate "\n" (map renderFacet fs) ++ "\nendsolid"

renderFacet :: Facet -> String
renderFacet f = printf "facet normal %s\n" (renderVertex $ normal f) ++
    "outer loop\n" ++ renderVerticies f ++ "endloop\nendfacet"
        where
            renderVerticies = foldMap (printf "vertex %s\n" . renderVertex)

renderVertex :: Vertex -> String
renderVertex = unwords . toList . fmap renderDouble

renderDouble :: Double -> String
renderDouble n
  | isInt precision n = printf "%d" (floor n :: Int)
  | otherwise         = printf ("%." ++ show precision ++ "f") n
    where precision   = 3

-- Returns if x is an int to n decimal places
isInt :: (RealFrac a) => Int -> a -> Bool
isInt n x = round (10.0 ^ n * (x - fromIntegral (round x :: Int))) == (0 :: Int)

normal :: Facet -> Vertex
normal (V3 a b c) = normalize $ cross (b - a) (c - a)

normalize :: V3 Double -> V3 Double
normalize v = v / pure ( sqrt $ sum $ v * v)

translate :: V3 Double -> Mesh -> Mesh
translate v = map (fmap (+ v))

up :: Double -> Mesh -> Mesh
up z = translate $ V3 0 0 z
