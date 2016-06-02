module Utils exposing (..)


resultAndMap : Result x (a -> b) -> Result x a -> Result x b
resultAndMap =
    Result.map2 (<|)
