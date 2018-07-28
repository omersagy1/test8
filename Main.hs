{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}


import Reflex.Dom
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Monoid


main =
  let
    doubleCountEvents eventCounter = foldDyn doubleCounter 0 (1 <$ eventCounter)
  in
    mainWidget $
          button "Click Me!" >>= (\c ->
          (count c) >>= (\tot ->
          (doubleCountEvents c) >>= (\tot2 ->
          (el "div" (display tot)) >>
          (el "div" (display tot2)))))


doubleCounter :: Integer -> Integer -> Integer
doubleCounter x accumulator =
  let
    g = h
    h = 2
  in
    accumulator + g * x
