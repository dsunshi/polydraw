
import Stl.Write
import Linear.V3
import qualified Data.ByteString.Lazy as B

pyramid :: Mesh
pyramid = [
            V3 t u v, -- base
            V3 t u w,
            V3 u v w,
            V3 v t w
          ]
              where
                  t = V3 0 0 0
                  u = V3 a 0 0
                  v = V3 (a/2) h 0
                  w = V3 (a/2) (2 * h / 6) a
                  h = a / 2 * sqrt 3
                  a = 50

main :: IO ()
main = do
    writeFile "pyramid.stl" $ renderStl pyramid
    B.writeFile "binary.stl"  $ putStl pyramid
