0.2.5.00x (current dev version)
===================

## Minor changes

- Fix a bug in `join_test-trace_data` when tests trigger no calls
- Fix `reload_pkg()` crashing on Windows library paths interpreted as invalid
  regular expressions

0.2.5
===================

## Major changes

- Added Antoine Soetewey (@AntoineSoetewey) as new contributor for #29.

## Minor changes

- Fix `insert_counters_in_tests()` crashing on ordinary `test_that()` descriptions (#29)

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
