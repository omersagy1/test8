module Model where


data Model = Model
  { val :: Int
  }

initModel :: Model
initModel = Model { val = 0 }