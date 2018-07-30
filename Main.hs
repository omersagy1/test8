{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

import Reflex.Dom
import qualified Data.Text as T
import qualified Data.Map as Map

import qualified Model
import qualified Render
import qualified Update


main :: IO ()
main = mainWidgetWithHead headElement appWidget


headElement :: MonadWidget t m => m ()
headElement = do
  el "title" (text "Test Program!")
  styleSheet "../css/simple.css"
  where
    styleSheet link = elAttr "link" (Map.fromList [
          ("rel", "stylesheet")
        , ("type", "text/css")
        , ("href", link)
      ]) $ return ()


appWidget :: MonadWidget t m => m ()
appWidget = mdo
  model <- foldDyn Update.update Model.initModel message
  message <- Render.render model
  return ()
