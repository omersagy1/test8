{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE InstanceSigs #-}


import Reflex.Dom
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Monoid


data Currency = USD | EUR

data Money = Money Int Currency

instance Show Money where
  show :: Money -> String
  show (Money val currency) =
    case currency of
      USD -> "$" ++ (show val)
      EUR -> "€" ++ (show val)

doubleMoney :: Money -> Money
doubleMoney (Money val currency) = (Money (val*2) currency)

dollars :: Int -> Money
dollars x = Money x USD

euros :: Int -> Money
euros x = Money x EUR


main :: IO ()
main = mainWidget root

root :: forall t m. MonadWidget t m => m ()
root =
  let
    doubleCountEvents eventCounter = foldDyn doubleCounter 0 (1 <$ eventCounter)
  in
    button "Click Me!" >>= (\c ->
    (count c) >>= (\tot ->
    (doubleCountEvents c) >>= (\tot2 ->
    (el "div" (display tot)) >>
    (el "div" (text (toText $ show $ Money 500 EUR))) >>
    (el "div" (display tot2)))))


doubleCounter :: Int -> Int -> Int
doubleCounter x accumulator =
  let
    g = h
    h = 2
  in
    accumulator + g * x


toText :: String -> T.Text
toText s = T.pack s