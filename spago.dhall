{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies = 
  [ "bytestrings"
  , "console"
  , "effect"
  , "foldable-traversable"
  , "generics-rep"
  , "lists"
  , "node-fs"
  , "node-process"
  , "partial"
  , "psci-support"
  , "refs"
  , "strings"
  ]

, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
