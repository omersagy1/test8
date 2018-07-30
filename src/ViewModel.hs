module ViewModel where

import Model

data ViewModel = ViewModel
  { counter :: Int
  }


fromModel :: Model -> ViewModel
fromModel model = ViewModel { counter = val model }