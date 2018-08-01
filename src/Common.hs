module Common where

-- Redefined piping operator from Elm/F#.
-- Equivalent to '&' in the Data.Function
-- module, but this form is used as a matter
-- of taste.
(|>) :: a -> (a -> b) -> b
(|>) = flip ($)
infixr 1 |>