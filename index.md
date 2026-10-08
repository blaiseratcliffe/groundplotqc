# groundplotqc

groundplotqc checks a set of related forest ground-plot tables against a
specification. It checks table and column structure, data types,
missing-value sentinels, code lists, keys and an applicability matrix.
It flags implausible values and changes between visits, proposes
corrections, links each issue to its root cause, and writes
self-contained HTML reports and CSV issue lists.

A generic engine works with any specification. A layer for the MAGPlot
2.0 ground-plot database supplies its compiled specification, rule set
and defaults.

## Status

Under development. This version has a specification reader, pre-flight
checks of the specification, and a rule set with settings
([`magp_rules()`](https://blaiseratcliffe.github.io/groundplotqc/reference/magp_rules.md),
and the `rules` and `settings` arguments of
[`gpq_preflight()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_preflight.md)).
The first release will still be version 0.1.0.

## Installation

Install the development version from GitHub:

``` r

# install.packages("pak")
pak::pak("blaiseratcliffe/groundplotqc")
```

## Links

- Documentation: <https://blaiseratcliffe.github.io/groundplotqc/>
- Report a problem:
  <https://github.com/blaiseratcliffe/groundplotqc/issues>
