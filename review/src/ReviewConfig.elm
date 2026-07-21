module ReviewConfig exposing (config)

{-| Isolated elm-review config for wire protocol extraction only.
-}

import ExtractWireProtocol
import Review.Rule exposing (Rule)


config : List Rule
config =
    [ ExtractWireProtocol.rule ]
