{-# LANGUAGE ScopedTypeVariables #-}
module Lens where


data Lens s a = Lens
  { getter :: s -> a
  , setter :: a -> s -> s }
