{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-map"
, dependencies =
    [ "console"
    , "effect"
    , "lists"
    , "maybe"
    , "ordered-collections"
    , "psci-support"
    , "transformers"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
