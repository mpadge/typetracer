0.2.4.00x (dev)
===================

## Bug fixes

- Fix `insert_counters_in_tests()` crashing (`'from' must be a finite number`) on
  ordinary `test_that()` descriptions containing regex metacharacters (e.g.
  `"foo() does x"`), apostrophes (e.g. `"doesn't"`), or a literal brace (e.g.
  `"aborts when {pkgname} isn't installed"`), by locating the test body via
  real parse-token positions instead of a text `grep()` of the description
  (#29)

0.2.4
===================

## Minor changes

- Fix bug in `pre_install()` to avoid compounding namespace database entries


0.2.3
===================

## Major changes

- Code for injecting tracers into function bodies entire updated as described in https://github.com/r-lib/covr/pull/587

0.2.2
===================

## Major changes

- `trace_package()` now reports environment from which traces were called (issue #14)
- Additional parameter added to `trace_package()` and `tracer_inject()` to allow recursive tracing into list structures (#19).

0.1.2
===================

## Initial CRAN Release
