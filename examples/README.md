# Strategy examples

These examples demonstrate common single-instrument signal patterns using the
instrument-neutral `.strategy` DSL. Each one accepts a `close : price` input and
emits a signed `exposure : weight`; the instrument is selected by the runtime.
They are illustrative templates, not performance claims.

- [EMA crossover](strategies/ema_crossover.strategy): long when the 12-period EMA
  is above the 26-period EMA, otherwise flat.
- [EMA trend filter](strategies/ema_trend_filter.strategy): full long above a
  50-period EMA, half short below it.
- [EMA mean reversion](strategies/ema_mean_reversion.strategy): half long below
  a 20-period EMA, otherwise flat.
- [Triple EMA trend](strategies/triple_ema_trend.strategy): scales exposure
  according to the alignment of 8-, 21-, and 55-period EMAs.

All examples are wrapped and compiled by Dune. Build them with:

```powershell
opam exec -- dune build examples/causalis_example_strategies.cma
```

The deterministic historical CSV used by the backtest integration test is at
[`data/demo_prices.csv`](data/demo_prices.csv). Run the full test suite, or the
focused strategy test, from the project root:

```powershell
opam exec -- dune runtest
opam exec -- dune runtest src/syntax/test/strategy_frontend_test.exe
```