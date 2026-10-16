# climatic4economist (development version, toward 2.0.0)

Version 2.0.0 is a redesign. The full design is in `dev/PLAN_v2.md` and
progress is tracked in `dev/LOG_v2.md` (both on branch `v2`).

## Planned changes

* Outputs become `c4e_tbl` tibbles carrying their context (variable, unit,
  time step, key, thresholds, percentiles, spell length, window), so later
  steps take their defaults from earlier ones.
* Rasters carry variable, unit and time step in terra's own slots, set by
  a new `read_climate()`.
* Four survey wrappers (`weather_extremes()`, `climate_indices()`,
  `static_controls()`, `time_varying_controls()`) and two grid wrappers
  (`grid_indicators()`, `grid_controls()`) with presets.
* Memory-aware automatic chunking with progress messages.
* `write_stata()` writes variable labels describing each indicator.
* v1 function names stay for one release as deprecated wrappers.

## Infrastructure

* Synthetic fixtures in `inst/extdata/` for tests and examples.
* R-CMD-check GitHub Actions workflow.

# climatic4economist 0.1.0

* First version.
