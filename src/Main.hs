
import Stl.Write
import Linear.V3

pyramid :: [Facet]
pyramid = [
            facet t u v, -- base
            facet t u w,
            facet u v w,
            facet v t w
          ]
              where
                  t = V3 0 0 0
                  u = V3 1 0 0
                  v = V3 (a/2) h 0
                  w = V3 (a/2) (2 * h / 6) 1
                  a = 1
                  h = a / 2 * sqrt 3

main :: IO ()
main = writeFile "pyramid.stl" $ renderStl pyramid
