module Model where


data Model = Model Int

initModel :: Model
initModel = Model 0

getVal :: Model -> Int
getVal (Model x) = x