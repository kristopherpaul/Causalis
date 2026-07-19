# Causalis

Causalis is a causal signal system and backtesting toolkit for systematic
strategies. A strategy describes how market observations become target exposure;
the runtime binds those observations and exposures to instruments, plans orders,
executes fills, and updates the portfolio.

## Instrument-neutral strategy

Strategies operate on named market inputs, not instrument identifiers. The same
compiled strategy can be run against different instruments by the backtest
runtime.

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

Weights are signed target exposures from `-100%` through `100%`. Negative
exposure opens a short position; trades still use nonnegative quantities. The
strategy emits only a weight. The backtest supplies the instrument and maps that
weight to its portfolio allocation.

## Build and test

From this directory, use the configured opam switch:

```powershell
opam exec -- dune build @all
opam exec -- dune runtest
```

See [the strategy syntax guide](src/syntax/README.md) for the standalone `.strategy`
build rule and generated module interface.