
module Stl.Write (Vertex, Facet, Mesh, translate, up, writeStlB, renderStl) where

import Linear.V3
import Text.Printf
import Data.List
import Data.Foldable
import Data.Int

import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.UTF8 as BLU
import Data.Binary.Put
import System.IO
import qualified Codec.Binary.UTF8.Generic as Da

-- Floating-point numbers are represented as IEEE floating-point numbers and are assumed to be little-endian, although this is not stated in documentation.

-- UINT8[80]    – Header                 -     80 bytes
-- UINT32       – Number of triangles    -      4 bytes

-- foreach triangle                      - 50 bytes:
--     REAL32[3] – Normal vector             - 12 bytes
--     REAL32[3] – Vertex 1                  - 12 bytes
--     REAL32[3] – Vertex 2                  - 12 bytes
--     REAL32[3] – Vertex 3                  - 12 bytes
--     UINT16    – Attribute byte count      -  2 bytes
-- end

header :: ByteString
header = BLU.fromString (printf "%-80s" "polydraw" :: String)

l :: Mesh -> Put
l m = putInt32le $ fromIntegral $ Data.Foldable.length m


writeStlB fname m = do
    h <- openFile fname WriteMode
    BL.hPut h header
    hClose h


type Vertex = V3 Double
type Facet  = V3 Vertex
type Mesh   = [Facet]

renderStl :: Mesh -> String
renderStl fs = "solid \n" ++ intercalate "\n" (map renderFacet fs) ++ "\nendsolid"

renderFacet :: Facet -> String
renderFacet f = printf "facet normal %s\n" (renderVertex $ normal f) ++
    "outer loop\n" ++ renderVerticies f ++ "endloop\nendfacet"
        where
            renderVerticies = foldMap (printf "vertex %s\n" . renderVertex)

renderVertex :: Vertex -> String
renderVertex = unwords . toList . fmap renderDouble

bFacet :: Facet -> Put
bFacet f = foldMap bVertex f <> putInt16le 0

bVertex :: Vertex -> Put
bVertex v = foldMap (putFloatle . realToFrac) v

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
