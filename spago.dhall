{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-map"
, dependencies = [ "console", "effect", "lists", "maybe", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
