{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecursiveDo #-}


import Reflex.Dom
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Monoid


data Message = Inc 
               | Dec
               | Zero

data Model = Model Int

initModel :: Model
initModel = Model 0

getVal :: Model -> Int
getVal (Model x) = x

update :: Message -> Model -> Model
update msg (Model val) =
  case msg of
    Inc -> Model (val + 1)
    Dec -> Model (val - 1)
    Zero -> Model 0


render :: forall t m. MonadWidget t m =>
          Dynamic t Model -> m (Event t Message)
render model = do 
  el "div" (display (fmap getVal model))

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

  model <- foldDyn update initModel message
  message <- render model
  return ()
