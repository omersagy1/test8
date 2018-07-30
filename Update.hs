module Update where

import Model
import Message


update :: Message -> Model -> Model
update msg (Model val) =
  case msg of
    Inc -> Model (val + 1)
    Dec -> Model (val - 1)
    Zero -> Model 0