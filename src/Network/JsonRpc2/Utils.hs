{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Network.JsonRpc2.Utils where

import Data.Char

mkJsonFieldName :: Int -> String -> String
mkJsonFieldName prefixLen name =
    let first:rest = drop prefixLen name
    in  toLower first : rest
