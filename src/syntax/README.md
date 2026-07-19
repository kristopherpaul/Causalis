# Strategy syntax

The `.strategy` frontend is a concise, instrument-neutral description of signal
logic. It declares named market input ports, derives causal signals, and emits a
signed target exposure. Instrument selection and order construction belong to
the backtest/runtime boundary, not to strategy expressions.

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

This trend strategy targets full long exposure when the faster EMA is above the
slower EMA and half short exposure otherwise. `weight` values range from `-100%`
to `100%`. The DSL provides price arithmetic, comparisons, `if/then/else`,
`pre`, `sma`, and `ema`; expressions lower automatically to the Signal API.
There is no explicit OCaml escape in the strategy source.

## Instrument binding

The strategy input is simply `close`; it never contains an instrument name. The
compiled artifact can be reused for another instrument by providing that
instrument's close trace and selecting its identifier at runtime. `Backtest.run`
converts each output weight into an allocation for its runtime `instrument`.
Negative weights create signed short positions; trade quantities remain
nonnegative, and covering uses the proceeds held as cash by the accounting model.

To compile a standalone file in a Dune project, generate an OCaml wrapper and
run the PPX on the generated module:

```lisp
(rule
(target ema_trend_generated.ml)
(deps ema_trend.strategy)
 (action
  (with-stdout-to %{target}
  (run %{bin:causalis-strategy-wrap} %{dep:ema_trend.strategy}))))

(library
 (name ema_trend)
 (modules ema_trend_generated)
 (libraries causalis.core causalis.strategy decimal)
 (preprocess (pps causalis.syntax.ppx)))
```

The generated `EmaTrend` module exposes typed handles as `input_<name>` and
`output_<name>`, packed `outputs`, output names, and `(port_name, input_id)`
metadata in `input_ports`. Its output type is `Domain.Weight.t`. A caller can
compile `outputs` once and pass `output_exposure` to `Backtest.run`, supplying
the generated close input as `inputs.close_price`. The runner turns each weight
into an instrument-keyed allocation; the planner derives intents from it.

`Weight` is a target exposure fraction: `1.0` is 100% long, `0.0` is flat, and
`-1.0` is 100% short. `Position` is signed to represent holdings, while order
`Quantity` remains nonnegative.