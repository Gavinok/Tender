{ name = "tender"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "effect"
  , "either"
  , "fetch"
  , "flame"
  , "maybe"
  , "prelude"
  , "psci-support"
  , "strings"
  , "web-html"
  , "yoga-json"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
