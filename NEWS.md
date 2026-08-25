0.2.4.00x (dev)
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
