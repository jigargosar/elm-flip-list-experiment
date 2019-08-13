module BasicsExtra exposing (callWith, eq_, unpackErr)

-- CORE HELPERS


unpackErr : (e -> v) -> Result e v -> v
unpackErr fn result =
    case result of
        Err e ->
            fn e

        Ok v ->
            v


callWith : a -> (a -> b) -> b
callWith =
    (|>)


eq_ : a -> a -> Bool
eq_ =
    (==)
