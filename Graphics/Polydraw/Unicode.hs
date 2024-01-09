{-# LANGUAGE UnicodeSyntax #-}

{-
Module      : Graphics.Polydraw.Unicode
Description : Unicode operators so you can write 'Model' expressions.
Copyright   : David Sunshine, 2024
              Mike Meyer, 2014
License     : BSD4
Maintainer  : david@sunshines.org
Stability   : experimental
-}

module Graphics.Polydraw.Unicode where

import Data.Semigroup ((<>))
import Graphics.Polydraw

infixl 6 ∪
infixr 6 ∩
infixl 9 ∖
infixl 9 ∆

-- | (&#x222A;) = 'union'
--
-- U+222A, UNION
(∪) :: Vector v => Model v -> Model v -> Model v
(∪) = (<>)

-- | (&#x2229;) = 'intersection'
--
-- U+2229, INTERSECTION
(∩) :: Vector v => Model v -> Model v -> Model v
a ∩ b = intersection [a, b]

-- | (&#x2216;) = 'difference'
--
-- U+2216, SET MINUS
(∖):: Vector v => Model v -> Model v -> Model v
(∖) = difference

-- | (&#x2206;) = Symmetric difference
--
-- U+2206, INCREMENT
(∆) :: Vector v => Model v -> Model v -> Model v
a ∆ b = (a ∖ b) ∪ (b ∖ a)

