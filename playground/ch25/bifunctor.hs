{-# LANGUAGE InstanceSigs #-}

module BifunctorExercise where

import Data.Bifunctor

data Deux a b = Deux a b
    deriving (Eq, Show)

instance Bifunctor Deux where
    bimap :: (a -> b) -> (c -> d) -> Deux a c -> Deux b d
    bimap f g (Deux x y) = Deux (f x) (g y) 