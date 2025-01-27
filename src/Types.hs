{-# LANGUAGE DeriveGeneric #-}

module Types (
    Entry (..),
) where

import GHC.Generics

data Entry = Entry {
        url_ :: String,
        processed_ :: Bool
    } deriving (Show)
