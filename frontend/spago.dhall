{ name = "tender"
, dependencies =
  [ "aff"
  , "arrays"
  , "bifunctors"
  , "effect"
  , "either"
  , "fetch"
  , "flame"
  , "maybe"
  , "prelude"
  , "tuples"
  , "psci-support"
  , "spec"
  , "strings"
  , "web-html"
  , "yoga-json"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
