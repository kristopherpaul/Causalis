# Causalis: Strategy Description Framework

Causalis is an OCaml-based typed synchronous language and runtime for systematic
trading strategies. Strategy programs describe causal signal computations and
compile into immutable execution graphs, keeping strategy meaning separate from
market data, order execution, and portfolio accounting.

The architecture is centered on a shared strategy artifact: a compiled causal
graph and state layout that can be interpreted by deterministic historical or
streaming executors. The current runnable path includes reference execution and
historical CSV backtesting; a streaming executor is a planned extension.

## Strategy example

Strategies declare instrument-neutral inputs and emit signed target exposure.
The selected instrument is provided by the runtime, not embedded in the signal
logic.

```text
strategy EmaTrend {
  input close : price;

  let fast = ema(12, close);
  let slow = ema(26, close);

  output exposure : weight =
    if fast > slow then 100%
    else -50%;
}
```

The faster EMA targets full long exposure above the slower EMA and half short
exposure otherwise. Weights are constrained to `-100%` through `100%`; holdings
are signed, while individual order quantities remain nonnegative.

## Compiler and runtime

The design calls for static type, clock, initialization, and causality checks.
Generated expressions receive OCaml type checking; the current causal compiler
checks clock compatibility and instantaneous cycles, while historical runs
report undefined outputs. Initialization analysis at compile time remains in
progress.

Strategy construction is available through ordinary OCaml Signal APIs and the
standalone `.strategy` frontend. See the [strategy syntax guide](src/syntax/README.md)
for frontend details and Dune integration.

## Try the repository

From the `Causalis/` project directory, build and run all tests:

```powershell
opam exec -- dune build @all
opam exec -- dune runtest
```

The focused strategy test also loads
[`examples/data/demo_prices.csv`](examples/data/demo_prices.csv), compiles the
instrument-neutral EMA strategy, and runs it through the historical backtest:

```powershell
opam exec -- dune runtest src/syntax/test/strategy_frontend_test.exe
```

The CSV is a small deterministic OHLCV sample with explicit RFC 3339 timestamps
and timezone offsets. The test verifies that the same strategy output is bound
to the runtime-selected instrument and can produce a short position.