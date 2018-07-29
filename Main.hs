{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecursiveDo #-}


import Reflex.Dom
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Monoid


main :: IO ()
main = mainWidget appWidget


appWidget :: forall t m. MonadWidget t m => m ()
appWidget = mdo
  model <- foldDyn update initModel message
  message <- render model
  return ()


data Model = Model Int

initModel :: Model
initModel = Model 0

getVal :: Model -> Int
getVal (Model x) = x


data Message = Inc 
               | Dec
               | Zero

update :: Message -> Model -> Model
update msg (Model val) =
  case msg of
    Inc -> Model (val + 1)
    Dec -> Model (val - 1)
    Zero -> Model 0


makeButton :: forall t m. MonadWidget t m =>
              T.Text -> Message -> m (Event t Message)
makeButton label callback = do
  clickEvent <- el "div" (button label)
  return (callback <$ clickEvent)


render :: forall t m. MonadWidget t m =>
          Dynamic t Model -> m (Event t Message)
render model = do 
  el "div" (display (fmap getVal model))

  incButton <- makeButton "Increment" Inc
  decButton <- makeButton "Decrement" Dec
  zeroButton <- makeButton "Zero Out" Zero

  return (leftmost [incButton, decButton, zeroButton])
