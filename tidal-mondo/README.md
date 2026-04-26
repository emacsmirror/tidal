# tidal-mondo
Convert [mondo][mondo] notation into tidal patterns.

[mondo]: https://strudel.cc/learn/mondo-notation/

## Differences with Tidal

- `scale` is applied to previous `note`: `note [0..12] # scale minor`
- prefixed modifiers like `fast` can be applied after: `s bd # fast 4`

Checkout the `itEval` examples from [mondo tidal test suite](./test/MondoTest.hs).

## Contribute

Run tests on changes with:

```
ghcid --command "cabal repl --enable-multi-repl tidal-mondo:test:tests lib:tidal-mondo" -W --test "hspec MondoTest.run"
```
