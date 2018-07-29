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
root = do
  clickEvent <- button "Click Me!" 
  tot <- count clickEvent
  tot2 <- doubleCountEvents clickEvent
  list <- simpleList (constDyn ["hello", "world"]) holdUniqDyn

  el "div" (display tot)
  el "div" (display tot2)


doubleCountEvents :: forall t m a. MonadWidget t m =>
                     Event t a -> m (Dynamic t Int)
doubleCountEvents eventCounter = foldDyn doubleCounter 0 (1 <$ eventCounter)


doubleCounter :: Int -> Int -> Int
doubleCounter x accumulator =
  let
    g = h
    h = 2
  in
    accumulator + g * x


toText :: String -> T.Text
toText s = T.pack s