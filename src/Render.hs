{-# LANGUAGE OverloadedStrings #-}

module Render where

import Reflex.Dom
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Monoid

import Message (Message)
import ViewModel (ViewModel)

import qualified Message
import qualified ViewModel


render :: MonadWidget t m =>
          Dynamic t ViewModel -> m (Event t Message)
render model = do 
  el "div" (display (fmap ViewModel.counter model))

  incButton <- clickableDiv "Increment" Message.Inc
  decButton <- clickableDiv "Decrement" Message.Dec
  zeroButton <- clickableDiv "Zero Out" Message.Zero

  return (leftmost [incButton, decButton, zeroButton])


clickableDiv :: DomBuilder t m =>
                T.Text -> Message -> m (Event t Message)
clickableDiv label callback = do
  (e, _) <- elAttr' "div" ("class" =: "btn") (text label)
  return (callback <$ domEvent Click e)

