{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecursiveDo #-}


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


data Message = Inc 
               | Dec
               | Zero

type Model = Int

initModel :: Model
initModel = 0

update :: Message -> Model -> Model
update msg model =
  case msg of
    Inc -> model + 1
    Dec -> model - 1
    Zero -> 0


render :: forall t m. MonadWidget t m =>
          Dynamic t Model -> m (Event t Message)
render model = do 
  el "div" (display model)

  clickEvent <- button "Increment"
  let incEvent = Inc <$ clickEvent

  clickEvent2 <- button "Decrement"
  let decEvent = Dec <$ clickEvent2

  clickEvent3 <- button "Zero Out"
  let zeroEvent = Zero <$ clickEvent3

  return (leftmost [incEvent, decEvent, zeroEvent])


main :: IO ()
main = mainWidget root


root :: forall t m. MonadWidget t m => m ()
root = mdo
  clickEvent <- button "Click Me!" 
  tot <- count clickEvent
  tot2 <- doubleCount clickEvent
  el "div" (display tot)
  el "div" (display tot2)
  el "div" blank
  mapM_ (el "div") (map text ["hello", "world"])

  -- Main loop.
  model <- foldDyn update initModel message
  message <- render model

  return ()


doubleCount :: forall t m a. MonadWidget t m =>
               Event t a -> m (Dynamic t Int)
doubleCount eventCounter = foldDyn doubleCounter 0 (1 <$ eventCounter)


doubleCounter :: Int -> Int -> Int
doubleCounter x accumulator =
  let
    g = h
    h = 2
  in
    accumulator + g * x


toText :: String -> T.Text
toText s = T.pack s