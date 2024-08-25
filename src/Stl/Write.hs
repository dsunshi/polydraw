
module Stl.Write (Vertex, Facet, renderStl) where

import Linear.V3
import Text.Printf
import Data.List
import Data.Foldable

type Vertex = V3 Double
type Facet  = V3 Vertex

renderStl :: [Facet] -> String
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

normalize :: Vertex -> Vertex
normalize (V3 x y z) = V3 a b c
    where
        l = sqrt (x * x + y * y + z * z)
        a = x / l
        b = y / l
        c = z / l
