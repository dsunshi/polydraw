
module Stl.Write (Vertex, Facet, facet, renderStl) where

import Linear.V3
import Text.Printf
import Data.List

type Vertex = V3 Double

data Facet = Facet Vertex Vertex Vertex

facet :: Vertex -> Vertex -> Vertex -> Facet
facet = Facet

normal :: Vertex -> Vertex -> Vertex -> Vertex
normal a b c = normalize $ cross (b - a) (c - a)

normalize :: Vertex -> Vertex
normalize (V3 x y z) = V3 a b c
    where
        l = sqrt (x * x + y * y + z * z)
        a = x / l
        b = y / l
        c = z / l

-- Returns if x is an int to n decimal places
isInt :: (RealFrac a) => Int -> a -> Bool
isInt n x = round (10.0 ^ n * (x - fromIntegral (round x :: Int))) == (0 :: Int)

renderDouble :: Double -> String
renderDouble n
  | isInt precision n = printf "%d" (floor n :: Int)
  | otherwise         = printf ("%." ++ show precision ++ "f") n
    where precision   = 3

renderVertex :: Vertex -> String
renderVertex (V3 x y z) = renderDouble x ++ " " ++ renderDouble y ++ " " ++ renderDouble z

renderFacet :: Facet -> String
renderFacet (Facet a b c) = printf "facet normal %s\n" (renderVertex $ normalize $ normal a b c) ++
    "outer loop\n" ++
    printf "vertex %s\n" (renderVertex a) ++
    printf "vertex %s\n" (renderVertex b) ++
    printf "vertex %s\n" (renderVertex c) ++
    "endloop\n" ++
    "endfacet"

renderStl :: [Facet] -> String
renderStl fs = "solid \n" ++ intercalate "\n" (map renderFacet fs) ++ "\nendsolid"

