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


clickableDiv :: DomBuilder t m =>
                T.Text -> Message -> m (Event t Message)
clickableDiv label callback = do
  (e, _) <- el' "div" (text label)
  return (callback <$ domEvent Click e)


render :: forall t m. MonadWidget t m =>
          Dynamic t Model -> m (Event t Message)
render model = do 
  el "div" (display (fmap getVal model))

  incButton <- clickableDiv "Increment" Inc
  decButton <- clickableDiv "Decrement" Dec
  zeroButton <- clickableDiv "Zero Out" Zero

  return (leftmost [incButton, decButton, zeroButton])
