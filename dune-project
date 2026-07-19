(lang dune 3.11)
(name causalis)
(package
 (name causalis)
 (synopsis "Causal Strategy DSL for Systematic Trading")
 (description "A typed, clocked, causal signal system for trading strategies")
 (authors "Causalis Team")
 (license MIT)
 (tags (trading dsl causal signals))
 (depends
  (ocaml (>= 4.14))
  (dune (>= 3.11))
  ppxlib
  decimal
  csv
    ptime
    alcotest))