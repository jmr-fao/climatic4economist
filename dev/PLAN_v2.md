# climatic4economist v2 — design plan

> **Source of truth.** Once chunk 0 lands, this document lives in the repo as
> `dev/PLAN_v2.md` on branch `v2`, next to `dev/LOG_v2.md` (progress log).
> The copy under `~/.claude/plans/` is disposable. Any session, on any laptop,
> starts by reading the log, then the plan section for the chunk it picks up,
> and ends by appending to the log (§15).

## Context

v1 (0.1.0, 77 exports) works, but context does not flow between steps. Nothing in
`R/` sets a class or attribute; the only context that travels is the unit key
(`resolve_key()` auto-detects `ID` / `ID_adm_div`) and whatever is encoded in
column names (`day_abv_30`, `mm_blw_0.1`, `spell_abv_30_90p`). Consequences seen
in the vignettes:

- Users retype the same values at every step: `unit = "celsius"` after converting
  Kelvin themselves, the wet-day cut `0.1` three times in one script
  (`sur_fre_weather_extreme.qmd`), `prefix = "tpr"` in `calc_par()` then
  `match_col = "tpr"` in `agg_to_adm_div()`, `interview`/`interval` in four
  consecutive index calls, `crs = "EPSG:4326"` and lon/lat names twice per script.
- `find_extr_spell_rel()` regex-parses thresholds back out of column names
  (`R/find_extr_spell_rel.R:26-66`) because nothing else remembers them.
- `compute_spi()` returns columns named exactly like its input, so SPI and
  precipitation tables are indistinguishable; the scale lives in object names.
- Time step is never recorded: `calc_pct_day()` assumes daily, `compute_spi()`
  assumes monthly, `calc_par()` discovers monthly input only by erroring; the
  ERA5 "monthly mean of daily totals" × 1000 pitfall is invisible.
- `sp_cntrl.qmd` writes `pre_avg` in metres to one file and in mm to another; a
  unit attribute would have caught it.

Decisions taken with the user (2026-10-16): clean break (v2.0.0, old names kept
one release as deprecated wrappers); tabular outputs become a tibble subclass with
a metadata attribute; raster stage uses terra's own slots set by a reader; scope
includes dedup, dropping the legacy sf/sp functions, Stata export with labels,
rewriting the vignettes, and general package hygiene.

Note on permissions: the OneDrive-level CLAUDE.md restricts writes to
`temp_file/`. Implementing this plan requires editing `R/`, `tests/`, `man/`,
`DESCRIPTION`, `NAMESPACE`, `vignettes/` of this package repo. Approving this plan
is taken as that explicit instruction; work happens on a `v2` branch, never on
`main` directly, and raw data folders are never touched.

## 1. The object model: `c4e_tbl`

Convention followed (the user asked which one applies): tibble's "Extending
tibble" vignette and `?dplyr::dplyr_extending`, the pattern sf, tsibble and
dtplyr use. A subclass built with `tibble::new_tibble()`, all metadata in one
attribute `c4e_meta`, and S3 methods so dplyr verbs keep it. Constructor /
validator / helper naming follows Advanced R (`new_c4e_tbl()`, `validate_meta()`,
`c4e_meta()`).

Governing rule: **capture at entry, rebuild at exit.** Every package function
reads `c4e_meta(x)` first, does its work (pivots, joins, summaries, which all
drop attributes), and wraps the result with `new_c4e_tbl()` carrying the updated
meta. The dplyr methods below only have to cover what the *user* does between
package calls.

New file `R/c4e_tbl.R`:

```r
new_c4e_tbl <- function(x, meta = list()) {
    x    <- tibble::as_tibble(x)                                   # strips data.table etc.
    meta <- validate_meta(utils::modifyList(empty_meta(), meta), x)
    tibble::new_tibble(x, c4e_meta = meta, nrow = nrow(x), class = "c4e_tbl")
}
c4e_meta    <- function(x) attr(x, "c4e_meta", exact = TRUE)       # NULL on plain tibbles
is_c4e_tbl  <- function(x) inherits(x, "c4e_tbl")
set_meta    <- function(x, meta) new_c4e_tbl(x, meta)
update_meta <- function(x, ...) set_meta(x, modify_meta(c4e_meta(x), rlang::list2(...)))  # NULL clears
add_indicator <- function(x, col, kind, ...) { m <- c4e_meta(x); m$indicators[[col]] <- new_indicator(kind, ...); set_meta(x, m) }
as_tibble.c4e_tbl <- function(x, ...) { attr(x, "c4e_meta") <- NULL; tibble::new_tibble(x, nrow = nrow(x)) }

# one chokepoint for every restoration
c4e_restore <- function(out, template) {
    m <- c4e_meta(template); if (is.null(m)) return(out)
    m$indicators  <- m$indicators[intersect(names(m$indicators), names(out))]   # prune, never error
    if (!is.null(m$key) && !m$key %in% names(out)) m$key <- NULL
    m$unit_cols   <- intersect(m$unit_cols, names(out))
    m$survey_cols <- intersect(m$survey_cols, names(out))
    new_c4e_tbl(out, m)
}
dplyr_reconstruct.c4e_tbl <- function(data, template) c4e_restore(NextMethod(), template)
`[.c4e_tbl`       <- function(x, i, j, ...) c4e_restore(NextMethod(), x)
`$<-.c4e_tbl`     <- function(x, name, value) c4e_restore(NextMethod(), x)
`[[<-.c4e_tbl`    <- function(x, i, value) c4e_restore(NextMethod(), x)
`names<-.c4e_tbl` <- function(x, value) { ... key, unit_cols, survey_cols, registry names,
                                            registry `base`/lookup thresholds follow the rename ... }
tbl_sum.c4e_tbl   <- function(x, ...) c(NextMethod(),
    c4e = "tmax [C], day, long; key `ID`; 4 indicators")
# vctrs: bind_rows()/vec_rbind() keep the class and merge meta
vec_ptype2.c4e_tbl.c4e_tbl <- function(x, y, ...) new_c4e_tbl(tibble::tib_ptype2(x, y, ...), merge_meta(c4e_meta(x), c4e_meta(y)))
vec_ptype2.c4e_tbl.tbl_df / vec_ptype2.tbl_df.c4e_tbl / vec_cast.* analogously
```

Registered with `@exportS3Method dplyr::dplyr_reconstruct` etc.; `vctrs` and
`cli` added to Imports.

What survives between package calls:

| Mechanism | Verbs | Result |
|---|---|---|
| `dplyr_row_slice()` | `filter`, `arrange`, `distinct`, `slice*` | kept, registry pruned |
| `dplyr_col_modify()` | `mutate`, `transmute`, `relocate` | kept; new columns are unregistered |
| `dplyr_reconstruct()` | `select`, `rename`, `rename_with`, `*_join` (x's meta), `bind_rows` (merged meta), `bind_cols` | kept |
| none | `summarise`, `reframe`, `count`, `tidyr::pivot_*`, `nest/unnest`, `as.data.frame`, `base::merge`, `data.table::*`, `arrow`/`fst` | lost → plain tibble; inside the package every such site is wrapped |

`group_by |> mutate |> ungroup` keeps meta (dplyr copies attributes into
`grouped_df` and `ungroup()` reconstructs); `group_by |> summarise` loses it,
which is fine because every package summariser rebuilds. `saveRDS()` keeps
attributes; `.dta` does not (§6). `c4e_meta()` checks a `schema` field so older
saved objects can be upgraded.

**Merge policy** (`merge_meta(a, b)`, used by joins inside the package,
`bind_rows`, and an exported `bind_indicators(...)` that replaces the
`purrr::reduce(full_join)` idiom in the vignettes): `key` must match (abort);
`indicators` union, same name with different entry → warn and keep `a`;
`variable`/`unit`/`time_step`/`transform` equal or one `NULL` → keep, otherwise
`NULL` with one message ("base series differ; per-indicator units retained");
`date_range` union; `source` union; `window`/`spatial_agg` equal or `a`'s with a
warning; `survey_cols` union. A bare user-level `full_join()` of two `c4e_tbl`
keeps only x's registry; documented, not an error.

**Fallback.** Every v2 function accepts a plain data.frame: `meta <-
c4e_meta(x) %||% empty_meta()`, `resolve_key()` runs its candidate lookup,
`calc_par`/`calc_std_index` fall back to the hard-coded id columns, and missing
context is an error naming the argument. The output is still a `c4e_tbl` built
from the explicit arguments, so a plain table is upgraded after one step: the
user retypes once, not at every step.

### `meta` schema

| Field | Type | Set by | Read as default by |
|---|---|---|---|
| `key` | chr(1) | `prepare_coord()`, `read_boundaries()`, `extract_*()` | every function (replaces repeated `resolve_key()`; still falls back to it) |
| `unit_cols` | chr | `extract_cell_by_poly()` (`x_cell`,`y_cell`,`coverage_fraction`) | `calc_std_index()`, `calc_par()`, `aggregate_to_adm()` |
| `variable` | chr(1) | `read_climate()` → `extract_*()`; user override | `calc_par(prefix)`, `aggregate_to_adm(match_col)`, column naming, Stata labels |
| `unit` | chr(1) | `read_climate()` / `convert_unit()` → `extract_*()` | threshold functions (column prefix, label, unit check) |
| `time_step` | "hour"/"day"/"month"/"year" | `read_climate()`, `aggregate_time()` → `extract_*()` | `calc_percentile()` (requires day), `calc_std_index()` (requires month), `calc_par()`, `summarise_by_interview()` |
| `shape` | "wide"/"long" | `extract_*()` (wide); first indicator (long) | indicator functions pivot once, automatically |
| `date_col` | chr(1) | the wide→long pivot | everything downstream (replaces hard-coded `date`) |
| `date_range` | Date(2) | `extract_*()`, `select_by_dates()` | printed; `calc_percentile()` records baseline |
| `crs` | chr(1) | `georef_coord()`, `extract_*()` | `get_poly_attr_for_point()`, checks in `extract_*()` |
| `source` | chr(1) | `read_climate()` (file/dataset name) | labels only |
| `spatial_agg` | list(fn, weights) | `extract_by_poly()`, `extract_weighted_by_poly()`, `aggregate_to_adm()` | labels only |
| `indicators` | named list, one entry per indicator column | every indicator function | `calc_spell()`, `calc_spell_excess()`, `summarise_by_interview()`, `aggregate_to_adm()`, `write_stata()` |
| `window` | list(interview, interval, n_lags) | `summarise_by_interview()`, `select_by_interview()` | `aggregate_to_adm()`, labels |
| `survey` | list(id, interview, lon, lat) | `prepare_coord()` / `prepare_survey()` | `georef_coord()`, `merge_with_survey()`, `summarise_by_interview()` |

`shape` takes `"wide"` (date columns), `"long"` (`date`, `value`), `"lookup"`
(key × month percentile table) or `"summary"` (key [, lag]). A `transform`
field (`list(index = "spi", scale = 3, base_variable = "pre")`) records
standardised indices; `survey_cols` records which columns came from the survey
(default `extra_col` for `aggregate_to_adm()`).

An `indicators` entry, keyed by column name:

```r
day_abv_30   = list(kind = "day",   base = "value",      direction = "above", threshold = 30,  variable = "tmax", unit = "C")
C_abv_30     = list(kind = "excess", base = "value",     direction = "above", threshold = 30,  unit = "C")
spell_abv_30 = list(kind = "spell", base = "day_abv_30", direction = "above", threshold = 30,  min_spell = 2)
day_90p      = list(kind = "pct",   base = "value",      p = 0.9, exclude_below = 0.1)
day_abv_90p  = list(kind = "day",   base = "value",      direction = "above", threshold = "day_90p", p = 0.9)  # chr = lookup column
spi3_blw_-1  = list(kind = "std",   direction = "below", threshold = -1, scale = 3)
pre_mean     = list(kind = "par",   stat = "mean", period = "year")
day_abv_30_sum = list(kind = "agg", base = "day_abv_30", stat = "sum")
```

`kind` is the closed set `day | excess | spell | pct | std | par | agg`. Each
entry carries its own `variable`/`unit`, so tables of different variables can be
merged without losing per-column meaning. Column names are the registry key
because that is what downstream selects on; the registry replaces the `^day`,
`abv|blw`, `[0-9]{2}p$` and `^day_abv_-?[0-9]{1,2}…` regexes. Labels are derived
on demand (`indicator_label()`), never stored, so they cannot go stale.

### Default-from-upstream idiom

```r
calc_spell <- function(x, min_spell = NULL, id = NULL) {
    meta      <- c4e_meta(x) %||% empty_meta()
    key       <- resolve_key(x, id)                       # v2: checks meta$key first
    min_spell <- c4e_default(min_spell, meta$min_spell, "min_spell", default = 2L)
    ...
    new_c4e_tbl(out, modify_meta(meta, min_spell = min_spell, indicators = ...))
}
```

`c4e_default(arg, from, arg_name, default = NULL, required = TRUE)`: explicit
value wins; else the meta value, **announced** with `cli::cli_inform("Using
{.arg min_spell} = 2 from upstream")`; else the hard default; else
`cli::cli_abort()` naming the argument and suggesting a v2 reader/extractor.
Announced rather than silent, because silent defaults recreate the current bug
class (nobody knows where the 0.1 came from). Whatever was used is written into
the outgoing meta, so an override at step 2 is the default at step 3.

Where an explicit value *conflicts* with meta (e.g. `unit = "K"` on a `"C"`
table) the function aborts and points at `convert_unit()`; no silent threshold
conversion.

Messages go through one `c4e_inform()` gated by
`getOption("climatic4economist.verbose", TRUE)` and by rlang's
`rlib_message_verbosity`. The same channel replaces the `iteration = ` /
`cat()` pattern copied into 16 functions: `iteration` stays one release as a
deprecated argument that only emits its label; docs recommend
`purrr::map(.progress = TRUE)`.

## 2. Raster stage (before extraction)

- `read_climate(path, variable = NULL, unit = NULL, time_step = NULL, source = NULL)`:
  `terra::rast()` + sets `terra::time()`, `terra::units()`, `terra::varnames()`,
  layer names = ISO dates. Detects ERA5 (`t2m_valid_time=`, `second_to_date()`
  logic), CHIRPS, WFDE5 layer naming; infers `time_step` from the time spacing.
  Replaces the `names(r) <- terra::time(r)` / `second_to_date()` boilerplate in
  every vignette.
- `convert_unit(x, to, from = NULL)`: S3 generic with methods for SpatRaster
  (reads/sets `terra::units()`), `c4e_tbl` (meta; converts date columns when
  wide, `value` plus `excess`/`pct` registry columns and their numeric thresholds
  when long; offsets apply to levels only, never to differences) and plain
  data.frame (`from` required). `R/units.R` holds an alias table (`degC`,
  `celsius`, `°C` → `C`; `kg m-2 s-1` → `kg/m2/s`) and a conversion table
  (K↔C, m→mm, kg/m2/s→mm/day, m/s→km/h); mm/day→mm/month needs `time_step`, so
  it errors unless the step is known. Replaces the eight hand-written
  `- 273.15` / `* 1000` / `* 86400` blocks.
- `aggregate_time(x, to = c("day","month","year"), fn)` (renames
  `aggregate_frequency()`): `fn` defaults from `variable` kind (precipitation
  → sum, temperature → mean) with override; updates `terra::time()` and the
  `time_step` meta; fixes the monthly `as_date(format="%Y-%m")` NA bug.
- `crop_with_buffer()`, `fill_missing_rast()`, `split_raster()`,
  `align_count_rast()` keep their behaviour; `fill_missing_rast(fun)` defaults to
  "modal" for categorical variables declared in `read_climate(kind = "categorical")`.

## 3. Function map v1 → v2

Prefix convention: `read_` ingest, `prepare_`/`georef_` survey side, `extract_`
raster→table, `select_` subsetting in time, `calc_` indicators, `summarise_`
survey-window summaries, `aggregate_` spatial/time aggregation, `merge_`,
`write_`. Argument convention: `x` first table, `above`/`below` for thresholds
(numeric or a percentile `c4e_tbl`), `p` percentile, `min_spell`, `scale`,
`interval`, `n_lags`, `id`, `fn` for aggregation functions, `na_rm`.

| v1 | v2 | Notes |
|---|---|---|
| `read_GAUL`, `read_geoBoundaries` | `read_boundaries(path, iso, level, source = c("gaul","geoboundaries"))` | shared pipeline; returns SpatVector with `ID_adm_div`; meta via `terra` attributes is not possible, so it also sets a `c4e_boundaries` S3 class on the returned list form only; level/iso recorded in `names()`/attributes of the SpatVector `values()` |
| `prepare_coord`, `georef_coord` | `prepare_coord(x, lon, lat, id = NULL, interview = NULL)`, `georef_coord(x, crs = "EPSG:4326")` | `georef_coord()` takes lon/lat from meta; `prepare_coord()` records survey id and interview columns |
| `extract_by_coord`, `extract_by_poly`, `extract_cell_by_poly`, `extract_weighted_by_poly` | same names | return `c4e_tbl` (wide) with variable/unit/time_step/date_range/crs/key; `extract_cell_by_poly` drops the `raster::brick` dependency; `extract_by_poly` gains `id`; CRS mismatch between raster and vector is an error |
| `aggregate_frequency` | `aggregate_time` | §2 |
| `select_by_dates`, `select_by_season`, `select_by_interview`, `filter_by_interview` | `select_by_dates`, `select_by_season`, `select_by_interview` | anchored date regex everywhere (`date_pattern()`); `filter_by_interview` folded into `select_by_interview(wide = FALSE)`; records `date_range` |
| `find_extr_abs_day` | `calc_threshold_day(x, above = NULL, below = NULL, unit = NULL)` | long output; `unit` from meta; registry entries `day_abv_30`, `<unit>_abv_30` |
| `calc_pct_day` | `calc_percentile(x, p, exclude_below = NULL, yearly = FALSE)` | renames the misleading `l_thresh`; output `c4e_tbl` with `kind = "pct"`, `p`, baseline `date_range` |
| `find_extr_rel_day` | `calc_percentile_day(x, pct, tail = NULL)` | `tail` inferred from `p` (≥ 0.5 upper) with override; checks `pct` and `x` share variable/unit/key |
| `find_spell` | `calc_spell(x, min_spell = NULL)` | iterates the registry (`kind == "day"`) instead of the `^day_abv_-?[0-9]{1,2}…` regex that skips thresholds ≥ 100 |
| `calc_pct_spell` | `calc_spell_percentile(x, p, min_spell = NULL)` | `min_spell` from meta |
| `find_extr_spell_rel` | `calc_spell_excess(x, pct)` | pairs each `kind == "pct"` entry of `pct` (whose `base` names the spell column) with the matching `kind == "spell"` entry of `x`; replaces the regex block at `R/find_extr_spell_rel.R:26-81`; aborts if no pair matches |
| `find_wmo_heatwave` | `calc_heatwave(x, excess = 5, min_spell = 2)` | checks `unit == "C"`; `na.rm` fix |
| `compute_spi`, `compute_spei`, `compute_water_balance` | `calc_std_index(x, scale, type = c("spi","spei"), pet = NULL)` | one function; requires `time_step == "month"`; output long with column `spi_3`; `compute_water_balance` becomes internal, matches by key+date not position |
| `extr_std_index` | `calc_std_threshold(x, above = NULL, below = NULL)` | keeps the minus sign; names `spi3_blw_-1.5` |
| `calc_par` | `calc_par(x, fn, period = NULL, min_coverage = 0.8)` | `prefix` from `variable`; records `kind = "par"` |
| `extr_day_index`, `extr_spell_index`, `extr_day_by_interview`, `extr_spell_by_interview` | `summarise_by_interview(survey, x, interval, n_lags = 0, id = NULL, interview = NULL, extra_col = NULL)` | one function; dispatches sum / max-mean-sum-n on registry `kind`; keeps the inequality-join implementation (`aggregate_by_interview()`, `make_lag_windows()`); `id`/`interview` from survey meta; records `window` |
| `merge_with_survey`, `merge_by_common`, `find_merge_var` | `merge_with_survey(survey, x, id = NULL)`, `merge_by_common(...)` (variadic, fixes the broken `y = NULL` form) | meta of `x` wins; survey fields merged in |
| `agg_to_adm_div` | `aggregate_to_adm(x, cols = NULL, extra_col = NULL)` | `cols` default = registry columns; weights `coverage_fraction` from `unit_cols` |
| `is_above`, `is_below`, `above_threshold`, `below_threshold`, `substitute_u`, `substitute_l` | internal `flag_threshold(x, threshold, direction, excess = FALSE)`, `mask_beyond(x, threshold, direction)` | not exported |
| `max/mean/sum_na_check`, `quantile_na_check`, `quantile_df` | internal `summarise_na(x, fn, replace)` | keep `cv`, `skewness`, `calc_mode` exported |
| `to_stata_format` | `write_stata(x, path)` + keep `to_stata_format()` | §6 |
| `ave_dist_2shp`, `ave_dist_2shp2`, `ave_point_density_2shp` | removed | superseded by `calc_distance()`, `calc_nearest_distance()`; they call sf/sp/raster unqualified and `sp` is not even imported |
| `second_to_date`, `character_to_clock`, `is_date`, `to_date`, `sort_date_columns` | internal date helpers (`parse_date_label()` is already the right one) | `check_dates_complete()`, `find_lag()` stay exported |
| path helpers (`check_path`, `find_folder`, `find_nested_folder`, `find_root_directory`, `find_subfolder`, `find_year_file`) | unchanged | out of the pipeline |
| geo analytics (`calc_distance`, `calc_nearest_distance`, `calc_road_density`, `calc_weighted_distance`, `get_utm_crs`, `erase_water_bodies`, `compare_adm_div`, `get_poly_attr_for_point`) | unchanged names | `get_poly_attr_for_point()` reads crs/geom from meta |

Every renamed v1 function stays as a thin wrapper calling the v2 function with
`lifecycle::deprecate_warn("2.0.0", "find_spell()", "calc_spell()")`, removed in
2.1.0.

## 4. Standardise return types and shapes

- All tabular returns are tibbles (`c4e_tbl`): `calc_pct_day`, `find_wmo_heatwave`,
  `find_extr_abs_day(unit = NULL)` and `compute_water_balance` currently return
  data.table / data.frame.
- Wide tables exist only between `extract_*()` and the first indicator; every
  indicator output is long with `date_col`. `calc_std_index()` therefore stops
  returning wide, which removes the manual pivot before `extr_std_index()`.
- `data.table` dropped from Imports; the four copies of the melt block become one
  internal `to_long()` that sets `shape`/`date_col`.
- Keys are character, dates are `Date` (existing invariants in
  `tests/testthat/test-invariants.R` extend to every function).

## 5. Package hygiene ("make it a proper package")

- `DESCRIPTION`: real Title (currently "…(Title Case)"), Description, author
  email (`fao.com` → `fao.org`), `Version: 2.0.0`, prune Imports (`raster`, `sf`,
  `data.table`, `stringi` out; `cli`, `lifecycle`, `vctrs` in), `Suggests`:
  `haven`, `quarto`, `knitr`, `testthat`, `exactextractr` becomes Suggests with
  `rlang::check_installed()`.
- `LICENSE` file matching `GPL-2`; `NEWS.md` with the v2 changelog and the
  deprecation table; `README.md` with a 15-line end-to-end example.
- `.github/workflows/R-CMD-check.yaml` and `pkgdown.yaml` (usethis templates);
  `_pkgdown.yml` with reference grouped by the §3 prefixes; vignettes stay a
  Quarto site but the 3 canonical ones also become package articles.
- `R CMD check` clean: `utils::globalVariables()` or `.data$` for NSE columns,
  no `cat()`, examples that run (several current examples reference objects that
  don't exist), `@keywords internal` on helpers.
- `@family` roxygen tags per prefix group; `@inheritParams` for the shared
  arguments (`x`, `id`, `above`, `below`, `min_spell`).
- Tests: a `test-c4e-tbl.R` for the class (reconstruct, subsetting, print,
  registry pruning, bind_rows, join, saveRDS round trip); a `test-pipeline.R`
  that runs synthetic raster → survey end to end and asserts no context argument
  is needed after the first call; existing tests migrate to the v2 names.

## 6. Export: `write_stata()`

`write_stata(x, path, labels = TRUE, ...)`: builds one label per registry column
with `indicator_label()` (e.g. `day_abv_30` → "days above 30 C (day, tmin)",
`spell_abv_30` → "spell length (>= 2 days) above 30 C", `spi3_blw_-1` → "spi-3
below -1", `day_abv_30_sum` → "sum of days above 30 C per window"), plus labels
for `value` and `lag` from `variable`/`unit`/`time_step`/`window`; sets
`attr(col, "label")` (truncated to Stata's 80 chars) **before**
`rename_with(to_stata_format)` so labels stay attached; then
`haven::write_dta()`. Column names ≤ 32 chars enforced with a clear error; the
`select(-5)` workaround in `afg_villages_cold_spell.qmd` disappears. `haven`
goes in Suggests. A reverse `read_stata()` (labels → meta) is out of scope for
2.0.

## 7. Vignettes

Rewrite to the v2 API, one script per canonical workflow, each running end to
end on a small bundled synthetic raster (`inst/extdata`) so they can also be
package articles:

1. Points: survey → `prepare_coord` → `georef_coord` → `read_climate` →
   `convert_unit` → `extract_by_coord` → `calc_threshold_day` → `calc_spell` →
   `summarise_by_interview` → `write_stata`.
2. Percentiles: `calc_percentile` → `calc_percentile_day` →
   `calc_spell_percentile` → `calc_spell_excess`.
3. Admin divisions: `read_boundaries` → `extract_cell_by_poly` → indicators →
   `aggregate_to_adm`.
4. Standardised indices: `calc_std_index` → `calc_std_threshold`.
5. Climatic parameters and spatial controls: `calc_par`, `extract_by_poly`,
   terrain/night-light/population controls.

The country case studies (afg, afr, lbr, sur) are updated to call v2 names; the
ones that call functions that no longer exist (`sur_fre_weather_extreme.qmd`,
`sur_spi.qmd` sourcing `functions.R`; `lbr_srvy_climatic_parameters.qmd` using
`agg_fns`) are fixed first since they are the ones a reader is most likely to
copy.

## 8. Road map: 17 chunks

Work happens on branch `v2`, one chunk per PR, merged into `v2` as it lands;
`v2` merges into `main` only at chunk 17. Every chunk ends with
`devtools::test()` green and nothing in `main`'s behaviour changed until chunk
13. Size: **S** fits one session, **M** one to two, **L** two to three. The
synthetic fixtures from chunk 0 are what every later test and benchmark uses, so
nothing depends on data outside the repo.

| # | Chunk | Size | Depends on | Deliverables | Done when |
|---|---|---|---|---|---|
| 0 | **Scaffold** | S | — | branch `v2`; `dev/PLAN_v2.md` (this document), `dev/LOG_v2.md` (§15 template), repo-root `CLAUDE.md` (§15), `^dev$` in `.Rbuildignore`; `DESCRIPTION` → `2.0.0.9000`, add `cli`, `lifecycle`, `vctrs`, `haven` (Suggests); `tests/testthat/helper-synthetic.R` making a daily (3 y × 2 vars) and a monthly (40 y) SpatRaster, 20 fake households, 3 polygons, stored under `inst/extdata/` as small `.tif`/`.gpkg`; `.github/workflows/R-CMD-check.yaml` (usethis template, allowed to fail for now); `NEWS.md` stub | CI runs; existing tests pass unchanged; plan, log and CLAUDE.md pushed so a second laptop can resume |
| 1 | **`c4e_tbl` class** | M | 0 | `R/c4e_tbl.R`: `new_c4e_tbl()`, `empty_meta()`, `validate_meta()`, `c4e_meta()`, `set_meta()`, `update_meta()`, `add_indicator()`, `c4e_restore()`, `dplyr_reconstruct`, `[`, `$<-`, `[[<-`, `names<-`, `tbl_sum`, `as_tibble`, vctrs `vec_ptype2`/`vec_cast`, `merge_meta()`, `bind_indicators()`; `test-c4e-tbl.R` | the attribute-survival test list in Verification passes; `print()` shows the context line |
| 2 | **Defaults, messages, history, chunking** | M | 1 | `R/c4e_meta.R`: `c4e_default()`, `c4e_inform()`, `verbose` option, `history` append (with elapsed time) + `c4e_history()`; `resolve_key()` meta fast path; `R/chunks.R`: `mem_budget()`, `estimate_bytes()`, `with_chunks()` (auto/forced/off, progress bar, optional `furrr`), `label` helper; `test-defaults.R`, `test-chunks.R` | explicit > meta > default > error, each with a snapshot of the message; `with_chunks()` under `options(mem_limit = "1MB")` splits, prints the split message and bar, and returns the unsplit result |
| 3 | **Units** | S | 1 | `R/units.R`: alias + conversion tables, `register_unit()`, `convert_unit()` generic with SpatRaster / `c4e_tbl` / data.frame methods, `canonical_unit()`; `test-units.R` | K→C on a raster and on a wide/long table give equal values; threshold conflict aborts |
| 4 | **Raster readers** | M | 3 | `R/read_climate.R` (+ `register_source()`, ERA5/CHIRPS/WFDE5/GAEZ conventions, time-step inference, 0–360 rotate, `kind`, `na_value`), `aggregate_time()` (replaces `aggregate_frequency()`), `select_by_dates()`/`select_by_season()` SpatRaster methods, `read_boundaries()` (merges `read_GAUL`/`read_geoBoundaries`); `test-read-climate.R` | synthetic files read with correct `time()`, `units()`, `varnames()`; monthly aggregation names are valid dates |
| 5 | **Extraction** | M | 4 | `extract_by_coord()` (+ `buffer`), `extract_by_poly()` (+ `id`, `weights = "coverage"`), `extract_cell_by_poly()` (exactextractr on terra, real `ID_adm_div`), `extract_weighted_by_poly()` all returning `c4e_tbl` with meta from terra slots; CRS/extent/NA checks; `raster` out of Imports; `test-extract.R` | meta of every extractor is complete (variable, unit, time_step, date_range, crs, key, unit_cols, spatial_agg); CRS mismatch errors |
| 6 | **Matrix kernel** | M | 1 | `R/kernel.R` (internal): every function `matrix -> matrix` with one row per unit/cell and dates as columns, so it plugs into both `calc_*.c4e_tbl` and `terra::app()`: `as_matrix_wide()`, `flag_threshold()`, `excess_threshold()`, `run_length_rows()` (spells; vectorised over rows, Rcpp fallback if profiling demands), `quantile_by_month()`, `lookup_by_month()`, `spi_matrix()`, `to_long()`; `inst/bench/bench-kernel.R`; `test-kernel.R` comparing against the v1 long-format functions on the synthetic data and against `terra::app()` output on the synthetic raster | exact equality with v1 results; same numbers through `app()`; 10k-unit benchmark recorded in `inst/bench/results.md` |
| 7 | **Daily indicators** | L | 2, 5, 6 | `calc_threshold_day()`, `calc_percentile()` (+ `baseline`), `calc_percentile_day()`, `calc_spell()`, `calc_spell_percentile()`, `calc_spell_excess()` (registry pairing, no regex), `calc_heatwave()`, `calc_condition()`, lookup-table thresholds; all with `label =` and `with_chunks()` by unit rows when the long table exceeds the budget; `test-indicators.R` migrated from `test-thresholds.R`, `test-spells.R`, `test-find-extr-rel-day.R`, `test-invariants.R` | v1 functions and v2 functions give identical tables on the fixtures; every output registers its columns; no context argument typed after `extract_*()` |
| 8 | **Standardised indices and parameters** | M | 2, 5 | `calc_std_index()` (matrix `SPEI::spi()`/`spei()`, internal water balance by key+date), `calc_std_threshold()` (minus sign kept, pivots wide input), `calc_par()` (`prefix` from meta, `period`); tests migrated from `test-compute-spi.R`, `test-compute-spei.R`, `test-calc-par.R` | SPI equals v1 per-unit result; scale recorded in `transform`; `calc_par` refuses a daily table asked for monthly aggregation with a clear message |
| 9 | **Survey side** | L | 2, 7 | `prepare_coord()` (stable ID, survey meta), `georef_coord()` (lon/lat/crs from meta), window objects (`window_lag()`, `window_season()`, `window_fixed()`, `window_since()`), `summarise_by_interview()` (one implementation, `stats`, `min_coverage`, dropped-interview report, memory estimate + `with_chunks()` by location), `summarise_by_period()`, `merge_with_survey()` explosion warning, `merge_with_survey()`, `aggregate_to_adm()` (registry-driven `cols`), `get_poly_attr_for_point()` meta; tests migrated from `test-extr-index.R`, `test-extr-by-interview.R`, `test-lag-and-interval.R`, `test-merge.R`, `test-aggregation.R` | the §7.1 chain runs on the fixtures with `interval` typed once; results equal v1 `extr_day_by_interview()` on every date except the documented leap-day case |
| 10 | **Export and inspection** | S | 9 | `write_stata()` (labels, 32-char check, `codebook = TRUE` sidecar), `indicator_label()`, `summary.c4e_tbl()`/`validate_c4e()`; `test-write-stata.R` | `haven::read_dta()` of the output shows the labels; sidecar lists every indicator and the history |
| 11 | **Wrappers and presets** | L | 8, 9, 10 | `R/wrappers.R`: `weather_extremes()`, `climate_indices()`, `static_controls()`, `time_varying_controls()`, each as `cli_progress_step()` stages with timings into history, `dry_run`, list-of-surveys mapping with `label` from names; `R/presets.R` + `list_presets()`; `extremes_control()`, `indices_control()`, `controls_control()` (incl. `chunk`, `dry_run`, `verbose`); `test-wrappers.R` | each wrapper equals its step-by-step chain on the fixtures; every preset runs on a matching fixture and errors cleanly on a mismatched one; `c4e_history()` output re-evaluates to the same table |
| 12 | **Grid methods and wrappers** | L | 6, 7, 8 | `SpatRaster` methods for `calc_threshold_day()`, `calc_percentile()`, `calc_percentile_day()`, `calc_spell()`, `calc_par()`, `calc_std_index()` using `tapp()`/`quantile()`/`focal()` where a built-in exists and the kernel via `app()` otherwise; `period` argument; `metags()` writer/reader (`set_raster_meta()`, `raster_meta()`), `extract_*()` restore meta from `metags()`; `grid_indicators()`, `grid_controls()` (surrounding-cell density, normals, optional `aggregate()`); `grid_first` option in `weather_extremes()`; `inst/bench/bench-grid.R`; `test-grid.R` | grid days-above per year equals `calc_threshold_day()` + yearly sum on the extracted table at the same cells; a `grid_indicators()` GeoTIFF read back with `terra::rast()` restores the registry through `extract_by_coord()`; a stack larger than `mem_info()` allows runs with `filename =` |
| 13 | **Deprecations and removals** | M | 7–12 | `R/deprecated.R`: every v1 name as a `lifecycle::deprecate_warn()` wrapper mapping old arguments (`u_thresh` → `above`, `l_thrshld` → `below`, `time_scale` → `scale`, `iteration` → message); delete `ave_dist_2shp*`, `ave_point_density_2shp`, `substitute_*`, `is_above`/`is_below`/`*_threshold`, `*_na_check` from exports; remove `data.table` from Imports; `test-deprecated.R` | every v1 example runs with exactly one lifecycle warning and the v1 values; `NAMESPACE` lists only v2 names plus wrappers |
| 14 | **Package hygiene** | M | 13 | `DESCRIPTION` (title, description, email, Imports pruned: `raster`, `sf`, `data.table`, `stringi`, `tidyterra` out; `exactextractr` to Suggests with `check_installed()`), `LICENSE`, `_pkgdown.yml` (reference grouped by prefix, wrappers first), `pkgdown.yaml` workflow, `globalVariables`/`.data`, `@family`/`@inheritParams`, all examples runnable on `inst/extdata` | `devtools::check()` 0 errors 0 warnings; CI and pkgdown green |
| 15 | **Quick start and canonical vignettes** | L | 12, 14 | `vignettes/quick_start.qmd` (the four survey wrappers and the two grid wrappers, one preset each) and the five canonical articles of §7 plus a sixth, "Grid indicators and maps", on `inst/extdata`; README example; `_targets.R` example | all render in CI from the bundled data |
| 16 | **Country case studies** | M | 15 | afg / afr / lbr / sur `.qmd` ported to v2 names (fix `sur_fre_weather_extreme`, `sur_spi`, `lbr_srvy_climatic_parameters`, `afr_sp_adm` first); `eval: false` where data is external; stale prose fixed | each file parses and its code uses only v2 exports (`lintr` object-usage check) |
| 17 | **Release 2.0.0** | S | 16 | `NEWS.md` final (new API, deprecation table, bugs fixed §9.8, benchmarks), version bump, tag `v2.0.0`, merge `v2` → `main`, README install line | `remotes::install_github()` of the tag passes the quick start |

Parallel tracks once chunk 2 is in: **A** = 3 → 4 → 5 (raster side), **B** = 6
(kernel), **C** = `prepare_coord`/`georef_coord`/window objects from 9 (they
need only chunk 2). Chunk 7 waits for A and B; 8 for A; the rest of 9 for 7; 11
for 8–10; 12 can run alongside 9–11 once 7 and 8 are in. Chunks 14 and 16 can
start while 15 is being written.

Each chunk's PR description carries: the §-reference of this plan it implements,
the v1 → v2 mapping rows it touches, and the benchmark line when the kernel or
an extractor changed.

## 9. Further improvements (added after review)

Recommended for 2.0 (each is small once the meta layer exists):

1. **Stable location IDs.** `prepare_coord()` builds `ID` with `cur_group_id()`
   over sorted lat/lon, so the same village gets a different ID whenever the row
   set changes; the afg vignettes rely on two files having been built from the
   same rows. v2 derives `ID` deterministically from rounded coordinates
   (`paste0(round(lon, 5), "_", round(lat, 5))`, or a short hash), so IDs match
   across waves, files and reruns. Recorded as `meta$id_rule`.
2. **Explicit baseline and relative date windows.** `calc_percentile(x, p,
   baseline = c("1981-01-01", "2010-12-31"))` records the baseline in the
   registry instead of relying on a prior `select_by_dates()`; and
   `select_by_dates()` gains a relative form (`before = interview, years = 30`
   or `survey_year`), replacing the per-survey start/end lists typed by position
   in the afr vignettes (and inconsistent between them: 1991–2021 vs 1992–2022).
3. **Missing-data policy in window summaries.** `sum_na_check()` turns a window
   with no data into 0 days above threshold. `summarise_by_interview()` gets
   `min_coverage` (share of expected days present, as `calc_par()` already has)
   and returns `NA` below it; the number of households dropped for a missing
   interview date is reported, not silent.
4. **Variable kind drives aggregation defaults.** `read_climate(kind =
   c("continuous", "count", "categorical"), na_value = NULL)`: `extract_by_poly`
   defaults to mean / sum / modal, `fill_missing_rast` to mean / mean / modal,
   `aggregate_time` to mean / sum / error; `na_value` masks codes like URCA's 128
   that no vignette currently handles. Fixes the URCA-averaged-with-mean mistake
   in `lbr_srvy_sp_control.qmd`.
5. **Extraction sanity checks.** `extract_*()` errors on CRS mismatch (already
   planned) and additionally warns with counts when points fall outside the
   raster extent or hit only NA cells, and detects 0–360 longitude rasters
   (extent xmax > 180) and rotates them, replacing the manual `terra::rotate()`
   in `afr_spei_spi.qmd`. `extract_cell_by_poly()` keys cells by the polygon's
   real `ID_adm_div` instead of its row position.
6. **Provenance.** Each package call appends `list(fn, args, time)` to
   `meta$history`; `c4e_history(x)` prints the pipeline as runnable code, and
   `write_stata(codebook = TRUE)` writes a sidecar `.md` with the labels and the
   history next to the `.dta`. Cheap to add once every function rebuilds meta,
   and what a team reviewer needs when a `.dta` lands on their desk.
7. **A `summary()` / `validate_c4e()` method** that reports, per table: date
   gaps (`check_dates_complete()` already exists), NA share per unit, units with
   no observations, and registry entries whose columns are all NA.
8. **Bugs fixed in passing** (tracked in NEWS): monthly labels parsed to NA by
   `clock::date_parse()` in `extr_*_index` and `filter_by_interview`;
   `aggregate_frequency()` monthly output names; `find_spell()` regex skipping
   thresholds ≥ 100 or with two decimals; `extr_std_index()` dropping the minus
   sign; `merge_by_common(y = NULL)` broken list form; `find_wmo_heatwave()`
   mean without `na.rm`; `compute_water_balance()` matching columns by position;
   `select_by_dates()`/`select_by_season()` unanchored year regex; the leap-day
   lag disagreement (v2 keeps the window-cut definition of
   `extr_day_by_interview()`, documented).

Deferred to 2.1:

- `read_stata()` reverse (haven labels → meta) so a `.dta` can re-enter the
  pipeline with context.
- Chunked extraction for rasters larger than memory (`split_raster()` exists;
  a `by_chunk = TRUE` on `extract_*()` with `furrr`).

## 10. Performance

The wide table is a units × dates matrix in disguise. v1 melts it to long and
runs grouped dplyr/data.table per unit for every indicator; for 10k units × 40
years of days that is 150M rows per step. v2 keeps a **matrix kernel** behind the
tidy interface:

1. **Threshold flags, excess, percentiles and spells on the matrix.**
   `as.matrix(x[date_cols]) > threshold` is one vectorised op; monthly
   percentiles are `apply` over month column blocks (or `matrixStats::rowQuantiles`);
   spells are a row-wise run-length kernel (`compute_spell()` vectorised across
   rows, or a 30-line Rcpp loop if the R version is too slow). Long output is
   produced once at the end by `to_long()`. Expected 10–50× over the current
   group_by path, and far less memory. The tidy long table stays the public
   shape; only the internals change.
2. **Fit SPI/SPEI on a matrix, not per unit.** `SPEI::spi()` accepts a
   multi-column matrix and fits all series in one call; `compute_spi()` currently
   nests per unit and dispatches each through `furrr::future_map`, paying worker
   serialisation per location. `calc_std_index()` pivots to a dates × units
   matrix, calls `SPEI::spi()` once (optionally `furrr` over column blocks for
   very wide inputs), and pivots back.
3. **Select dates on the raster, before extraction.** `select_by_dates()` gets
   a SpatRaster method using `terra::time()`, so a 30-year baseline is extracted
   instead of the full 80-year file. Same for `select_by_season()`.
4. **Direct `exactextractr` on terra objects.** `extract_cell_by_poly()`
   converts to `raster::brick` first; `exactextractr` has taken `SpatRaster`
   since 0.8, so the conversion (a full copy) goes, and `raster` leaves Imports.
   When no per-cell indicator is needed, `extract_by_poly(fn = "mean", weights =
   "coverage")` does the coverage-weighted mean in C++ rather than materialising
   cells and calling `aggregate_to_adm()` afterwards.
5. **Window summaries stay on the inequality join.** `summarise_by_interview()`
   keeps the distinct-windows + `join_by(between())` + broadcast design of
   `extr_day_by_interview()`, never the merge-first path, and it is the only
   implementation. Very large surveys can be processed by chunks of `by` key
   (`chunk_size`) to bound memory.
6. **Pivot cost.** Dropping `data.table` loses its fast `melt`; `to_long()`
   uses `tidyr::pivot_longer()` only at the output boundary (once per
   indicator chain instead of once per function), so the net is still faster.
   If profiling shows the pivot dominating, `collapse::pivot()` is an optional
   accelerator behind `rlang::is_installed()`.
7. **Benchmarks in `inst/bench/`** with `bench::mark()` on the synthetic data at
   three sizes (1k / 10k / 100k units), run before and after phase 3 so the
   matrix kernel is measured, not assumed.

## 11. Flexibility

1. **Thresholds are either numbers or lookup tables.** `above`/`below` accept
   a numeric vector, or any `c4e_tbl` keyed by unit (and optionally month) with
   one column per threshold. Percentile tables are just one producer of such a
   table; a user can supply agro-ecological zone thresholds, per-district heat
   thresholds, or crop-specific limits through the same code path (registry
   `threshold = "<lookup col>"`).
2. **User-defined conditions.** `calc_condition(x, hot_humid = value > 30 &
   rh > 60, frost = value < 0)` registers `kind = "day"` columns from arbitrary
   data-masked expressions, so `calc_spell()`, `summarise_by_interview()`,
   `aggregate_to_adm()` and `write_stata()` treat them exactly like built-ins.
3. **Window specifications beyond relative lags.** `summarise_by_interview()`
   takes a `window` object: `window_lag("1 year", n_lags = 2)` (today's
   behaviour), `window_season(start = 6, end = 9, years_back = 0:2)` for growing
   seasons, `window_fixed(from, to)` for a calendar period, `window_since(event
   = date_col)` for time since an event. All produce the same `(key, interview,
   lag, w_start, w_end)` table that `make_lag_windows()` already builds, so the
   join code is unchanged. Also usable without a survey: `summarise_by_period(x,
   "month" | "year" | "season")` for panel outputs.
4. **Custom summary statistics.** `stats = list(sum = sum, max = max, p90 =
   \(v) quantile(v, .9))` per indicator kind, with the registry recording
   `stat`; defaults stay sum for days, max/mean/sum/n for spells, mean for
   parameters.
5. **Counting unit follows the time step.** On hourly data the flag columns are
   `hr_abv_30` and labels say "hours above"; on monthly data `mo_abv_…`; the
   registry `kind = "day"` becomes `kind = "flag"` with `step` recorded, so heat
   stress hours or dry months need no new functions.
6. **Buffers around points.** `extract_by_coord(buffer = 10000, fn = "mean")`
   extracts the neighbourhood of each location (done by hand with
   `terra::buffer()` + `extract_by_poly()` in `afg_village_control.qmd`);
   `fn` defaults from variable kind.
7. **Extensible registries.** `register_unit(from, to, factor, offset)` and
   `register_source(name, time_from, unit, variable_from)` let a project add a
   conversion or a dataset naming convention (TerraClimate, CMIP6, CHIRTS,
   AgERA5) without a package release; built-ins cover ERA5/ERA5-Land, CHIRPS,
   WFDE5, GAEZ.
8. **Column arguments: one convention.** Survey-side columns (`id`,
   `interview`, `lon`, `lat`, `extra_col`) are tidy-select / data-masking
   throughout; names that travel in meta (`key`, `date_col`) are strings. Today
   `id` means a string key in most functions and a bare household column in the
   `extr_*` family.
9. **Pure functions, rds-safe objects.** Because every output is a plain R
   object whose meta survives `saveRDS()`, the pipeline drops straight into
   `targets` for caching and reruns; the vignettes show one `_targets.R` for the
   points workflow.

Deferred to 2.1: multi-variable tables (tmax + rh in one `c4e_tbl` from a list
of rasters) beyond what `calc_condition()` on a joined table already allows;
`collapse` as a grouped-ops backend; chunked raster extraction.

## 12. High-level wrappers: four entry points

The step functions (§3) are the building blocks; most users should not need
them. Four wrappers run the whole chain from a survey and a spatial object
with a short argument list, sensible defaults, and named presets. Each returns a
`c4e_tbl` whose `c4e_history()` prints the equivalent step-by-step code, so a
user who needs to deviate can copy that and edit one line.

| Wrapper | Question it answers | Output grain |
|---|---|---|
| `weather_extremes()` | how many extreme days/spells hit each household before its interview | `id` × lag |
| `climate_indices()` | how dry/wet was it (SPI/SPEI) before each interview | `id` × lag, or `id` × month when `wide = TRUE` |
| `static_controls()` | what is this location like (terrain, zone, distances) | `id` |
| `time_varying_controls()` | what was this location like in the survey year (population, lights, normals) | `id` × year or survey wave |

### `weather_extremes()`

```r
weather_extremes(
    survey, climate,                       # data frame; SpatRaster, path, or c4e_tbl from extract_*()
    lon, lat, id, interview,               # survey columns (tidy-select); lon/lat/interview taken from meta if prepare_coord() ran
    preset   = NULL,                       # "heavy_rain", "dry_spell", "heat", "cold", "wet_days_p95", ... (see below)
    above    = NULL, below = NULL,         # absolute thresholds, in the data unit
    p        = NULL,                       # percentiles -> relative thresholds (baseline = full series unless `baseline` given)
    spells   = TRUE, min_spell = 2,        # add spell indicators for every flag
    window   = window_lag("1 year", n_lags = 0),
    boundaries = NULL,                     # SpatVector -> cell extraction + aggregate_to_adm(); NULL -> point extraction
    baseline = NULL, unit = NULL,
    control  = extremes_control()          # everything else: exclude_below, yearly, fill_missing, buffer, keep_steps
)
```

Chain: `prepare_coord → georef_coord → [read_climate] → [convert_unit] →
select_by_dates(baseline ∪ window span) → extract_by_coord | extract_cell_by_poly
→ calc_threshold_day / calc_percentile + calc_percentile_day → [calc_spell →
calc_spell_percentile → calc_spell_excess] → summarise_by_interview →
[aggregate_to_adm]`. Output: one row per `id` × lag with every indicator,
registry and window recorded. Rules that keep the arguments few:

- `above`/`below`/`p` decide the typology; nothing else is asked. Giving `p`
  with precipitation sets `exclude_below = 0.1` (wet days) unless overridden.
- `unit` is read from the raster; if the raster says `K` and the thresholds
  look like °C (`above < 100`), convert and say so, rather than error, because
  this is the one place the user has not opted into the step-by-step API.
- `spells = TRUE` adds spell length, spell percentiles when `p` is given, and
  the max/mean/sum/n window summaries; `spells = FALSE` gives days and excess
  only.
- `control` is a list constructor (`glm.control()` style) for the long tail,
  so the signature stays readable and `?weather_extremes` fits on one screen.

### `climate_indices()`

```r
climate_indices(
    survey, pre, pet = NULL,               # monthly rasters; pet switches SPI -> SPEI
    lon, lat, id, interview,
    index = c("spi", "spei"), scale = c(1, 3, 6),
    below = c(-1, -1.5, -2), above = NULL,
    window = window_lag("1 month", n_lags = 0),   # or wide = TRUE to return the series around the interview
    boundaries = NULL, baseline = NULL, control = indices_control()
)
```

Chain: georef → read/convert → extract → `calc_std_index()` per scale →
`calc_std_threshold()` → `summarise_by_interview()` | `select_by_interview(wide
= TRUE)` → [aggregate_to_adm]. Replaces the `afg_village_spi`, `lbr_*_spei`,
`afr_spei_spi` and `sur_spi` scripts.

### `static_controls()`

Layers with no time dimension: elevation and terrain, agro-ecological zone,
URCA, soil, land cover, distances to roads/markets/water.

```r
static_controls(
    survey, layers,                        # named list: list(elevation = dem, aez = r, urca = r, roads = v)
    lon, lat, id,
    kind = NULL,                           # per layer: "continuous" | "count" | "categorical"; guessed from read_climate()/names
    buffer = 0,                            # metres; vector -> one set of columns per radius (afg_village_control)
    terrain = c("slope", "TRI", "roughness"),   # computed when an `elevation` layer is present
    distance_to = NULL,                    # named list of SpatVectors -> calc_nearest_distance() in km
    boundaries = NULL, control = controls_control()
)
```

Chain: georef → per layer `fill_missing_rast(fun by kind)` →
`extract_by_coord(buffer)` | `extract_by_poly(fn by kind)` → terrain →
`calc_nearest_distance()` → one wide `c4e_tbl` keyed by `id` (or `ID_adm_div`),
one row per location. Layer kind is checked: a categorical layer is never
averaged (the URCA mistake), and `na_value` masks no-data codes.

### `time_varying_controls()`

Layers that exist per year or per month, where the right slice depends on the
survey: population, night lights, NDVI, land cover by year, and the climate
normals from `calc_par()`.

```r
time_varying_controls(
    survey, layers,                        # named list; each a multi-layer SpatRaster with time(), or a list of rasters by year
    lon, lat, id, interview,               # or `year =` a survey-year column
    match = c("same_year", "previous_year", "nearest"),   # which layer to take for each survey year
    climate = NULL, years = 30,            # list(pre = r, tmp = r) -> calc_par() normals over `years` before the survey year
    stats   = c("mean", "sd", "cv"),       # for the normals
    density = TRUE, log = c("nightlight"), # derived columns: pop / expanse(), log1p()
    buffer = 0, boundaries = NULL, control = controls_control()
)
```

Chain: georef → survey year from `interview` → per layer pick the slice by
`match` (replaces the per-survey hand-written year choice in `afr_sp_coord`,
`afr_sp_adm`, `lbr_srvy_adm_div`, and the rule stated in prose but not applied
in code) → `extract_*()` → `calc_par()` on the climate layers over
`[survey_year - years, survey_year)` → derived columns → one wide `c4e_tbl`
keyed by `id` × `survey_year`. With a panel (`survey` a list or a `wave`
column) each wave gets its own slice automatically. Meta records which layer
year served which survey year, and `write_stata()` labels say so
("population 2021 (survey 2022, previous_year)").

Together the two replace the five hand-written control scripts
(`afg_village_control`, `afr_*`, `lbr_srvy_sp_control`, `sp_cntrl`).

### Presets (`R/presets.R`)

A named list of specs that fill `above`/`below`/`p`/`min_spell`/`exclude_below`
and the expected variable kind, so the user picks a word instead of five
numbers. `list_presets()` prints them; a preset is overridden by any argument
given explicitly, and the preset name is recorded in meta and in Stata labels.

| preset | variable | spec |
|---|---|---|
| `heavy_rain` | precipitation (mm/day) | `above = c(20, 50)` |
| `wet_days_p95` | precipitation | `p = c(0.95, 0.99)`, `exclude_below = 0.1` |
| `dry_spell` | precipitation | `below = 1`, `min_spell = 5` |
| `dry_days_p10` | precipitation | `p = 0.10` |
| `heat` | tmax (C) | `above = c(30, 35)` |
| `heatwave_wmo` | tmax | `calc_heatwave(excess = 5, min_spell = 2)` |
| `hot_days_p90` | tmax | `p = c(0.90, 0.95)` |
| `cold` | tmin (C) | `below = c(0, -5)` |
| `cold_days_p10` | tmin | `p = c(0.10, 0.05)` |
| `drought_spi3` | precipitation (monthly) | `climate_indices(index = "spi", scale = 3, below = c(-1, -1.5, -2))` |

A preset checks the raster's `variable`/`unit` against what it expects and
errors with a one-line fix if they differ ("`heat` expects daily tmax in C;
the raster carries t2m_min in K").

### Behaviour shared by the four wrappers

- `keep_steps = TRUE` (in `control`) returns a named list of every intermediate
  table instead of only the final one, for debugging and for the vignette that
  walks through the chain.
- Each wrapper prints a short specification block up front (variable, unit,
  time step, thresholds, window, n units, n households) and then progress
  through `c4e_inform()`; `verbose = FALSE` silences both.
- `survey` may be a list of surveys (the afr multi-country pattern); the wrapper
  maps over it with `purrr::map(.progress = TRUE)` and binds with
  `bind_rows(.id = "survey")`, merging meta.
- Wrappers never write files; `write_stata()` is the explicit last call.

## 13. Pixel-level computation (grid outputs, no survey)

The same indicators, controls and parameters computed for every cell of a
raster, returning rasters: one layer per period (year, month, season) for the
extremes, one layer per statistic for the normals, one layer per radius for the
surrounding-population density. Uses: maps, panels of admin units or grid cells,
and a cheaper route for the survey path when many surveys share one raster
(compute per-year counts once on the grid, then `extract_*()`).

### terra or array?

Both, through one kernel. `terra::app(x, fun)` reads the stack in blocks and
passes `fun` a matrix with one row per cell and one column per layer; if `fun`
is vectorised over rows (returns a matrix with the same number of rows) terra
calls it once per block, writes the block, and moves on. That is exactly the
units × dates matrix kernel of §10, so the kernel functions are written once as
`matrix -> matrix` and reused by `calc_*.c4e_tbl` (rows = units from
extraction) and `calc_*.SpatRaster` (rows = cells from `app()`). Memory safety,
`cores =` parallelism and `filename =` output come from terra.

| Operation | Best route | Why |
|---|---|---|
| days above/below a fixed threshold per period | `terra::tapp(x > t, index = "years", fun = "sum")` | layer algebra + built-in `sum`: all C++, no R callback |
| excess over threshold per period | `tapp(ifel(x > t, x - t, 0), "years", "sum")` | same |
| long-run mean / sd / min / max / cv (climatic parameters) | `app(x, "mean")`, `stdev(x)`, raster algebra | built-in |
| per-cell percentiles over a baseline | `terra::quantile(x[[month_layers]], probs)` | C++ across layers; subset layers by month first |
| flags against month-specific percentile lookups | kernel via `app()` | needs a per-row, per-month lookup; no built-in |
| spells (longest spell, n spells ≥ min, spell days) per period | kernel via `app()` (row-wise run length) | no built-in; vectorised over rows, so one call per block |
| SPI / SPEI per cell | kernel via `app()` with `SPEI::spi()` on the block matrix (cells as columns) | distribution fitting; blocks keep memory bounded |
| population of surrounding cells | `focal(pop, w = focalMat(pop, d_m, "circle"), fun = "sum")` ÷ `focal(cellSize(pop), w, "sum")` | C++ moving window in metres |
| coarser-grid controls | `terra::aggregate(fact, fun)` | built-in |
| point density, distance to features | `terra::distance()`, `rasterize()` + `focal` | built-in |

Full in-memory conversion (`terra::values()` → kernel → `terra::setValues()`)
is kept only as an internal fast path chosen when `terra::mem_info(x)` says the
stack fits, since it saves the block overhead but nothing else; the public
behaviour is identical. Never `terra::extract()` on every cell centre to get a
grid: `values()` or `app()` only.

Output rasters carry `time()` (period start), `units()` ("days", "mm", "sd"),
`varnames()` (indicator column name) and the registry entry serialised into
`terra::metags()`, which round-trips through GeoTIFF. `extract_*()` reads
`metags()` back into `c4e_meta`, so a grid indicator extracted to survey points
arrives with its threshold, unit and period already known. Counts are written
as `INT2U` with ZSTD compression through `wopt`.

### API

The `calc_*` functions of §3 become S3 generics with `c4e_tbl` and `SpatRaster`
methods sharing the kernel; a `period` argument (`"year"`, `"month"`,
`"season"`, or a `window_*()` object) replaces the survey window on the grid.
Two grid wrappers complete §12:

```r
grid_indicators(climate, preset = NULL, above = NULL, below = NULL, p = NULL,
                spells = TRUE, min_spell = 2, period = "year",
                baseline = NULL, unit = NULL, filename = NULL, control = extremes_control())
# -> SpatRaster: one layer per indicator × period, metags with the registry

grid_controls(pop = NULL, radius_m = c(5000, 10000),       # surrounding-cell density per radius
              climate = NULL, years = 30, stats = c("mean", "sd", "cv"),   # per-cell normals
              layers = NULL, kind = NULL, fact = NULL,     # other layers, optional aggregate()
              filename = NULL, control = controls_control())
# -> SpatRaster: pop_density_5km, pop_density_10km, pre_mean, pre_sd, ..., metags
```

Both accept `filename =` and write block-wise, so country-scale daily stacks
never need to fit in memory. `weather_extremes()` gains `grid_first = TRUE`
(in `control`) to compute per-year counts on the grid and extract them, for
the case of many surveys over one raster; it is not the default because
per-interview windows need daily resolution.

## 14. Memory safeguards and progress

### Budget and estimate

- `mem_budget()` returns `getOption("climatic4economist.mem_limit")` if set
  (a number of bytes or a string like `"8GB"`), else 50 % of
  `terra::free_RAM()` (already a dependency, no new import).
- Every heavy function estimates its largest intermediate before allocating
  it, with `estimate_bytes(n_rows, n_cols, type = "double")`:

| Function | Largest intermediate | Estimate | Split by |
|---|---|---|---|
| `to_long()` / indicator chain | long table units × dates × (1 + n indicators) | `n_units * n_dates * (1 + k) * 8` | rows of the wide matrix (units); the kernel is row-independent |
| `summarise_by_interview()` | inequality join: windows × days inside them × indicator columns | `n_windows * mean_window_days * k * 8` | location key: distinct `by` values, each chunk's households complete |
| `calc_std_index()` | dates × units matrix plus the fitted copy | `2 * n_dates * n_units * 8` | columns (units) |
| `calc_percentile()` | baseline long table | as `to_long()` | units |
| `extract_*()` | points/cells × layers result | `n_points * n_layers * 8` | layers (date blocks), then `cbind`; terra already blocks the raster read |
| `merge_with_survey()` on a long table | households × dates explosion | `sum_over_keys(n_hh_key * n_dates) * ncol * 8` | not split: warn above budget and point to `summarise_by_interview()`, which never builds it |
| `aggregate_to_adm()` | cells × lags × indicators | `n_cells * n_lags * k * 8` | admin unit |
| grid `calc_*.SpatRaster` | terra blocks | handled by `terra::app()` / `wopt`; `filename` forced when `mem_info()` says it does not fit | terra |

### Automatic splitting

One internal helper runs every split:

```r
with_chunks(x, by = NULL, chunk = "auto", fn, label, mem_estimate)
# chunk = "auto": split only when mem_estimate > mem_budget(), into ceiling(estimate / budget) pieces
# chunk = <integer>: force that many units per chunk;  chunk = FALSE: never split (error if over budget)
```

Behaviour when splitting occurs:

```
ℹ Window summaries need ~12.4 GB (1,234 households × 15,706 days × 6 indicators); budget is 8 GB.
  Splitting 312 locations into 2 chunks.
  Window summaries ■■■■■■■■■■■■■■■■■■■■■■■■■■□□□□  chunk 2/2 | 00:41 | ETA 00:39
```

The message states the size, the budget, what is being split and into how many
pieces; the bar is `cli::cli_progress_bar()` updated per chunk, with elapsed
time and ETA. Results are bound with `bind_rows()` (meta merged) and are
identical to the unsplit result, which is what the test checks. Every heavy
function exposes `chunk =` so a user can force or forbid splitting; the
`control()` lists of the wrappers pass it through. `with_chunks()` also accepts
`.parallel = TRUE` to map chunks with `furrr` when a `future::plan()` is set,
since chunks are independent.

A `dry_run = TRUE` entry in every wrapper's `control()` prints the step plan,
the estimated size of each step and whether it would split, and returns without
computing, so a user can size a job before launching it on a laptop.

### Progress

- **Per call, in `purrr` loops.** `label = NULL` (the successor of
  `iteration`) prints `cli::cli_progress_step("Computing spells: {label}")`
  at the start of a call. When `label` is `NULL` the function uses
  `meta$label`, set by the wrappers from `names()` when they map over a list of
  surveys and by `read_climate()` from the file name, so
  `purrr::map(surveys, weather_extremes, ...)` prints each survey's name
  without an extra argument. `iteration =` keeps working one release as an
  alias with a deprecation note. Docs also recommend `purrr::map(.progress =
  TRUE)` for the outer loop.
- **Per step, in the wrappers.** Each wrapper runs its chain as
  `cli::cli_progress_step()` stages, so the console reads:

```
── weather_extremes(): Malawi19 ────────────────────────────────
✔ Georeferenced 1,234 households at 312 locations (EPSG:4326)          [0.2s]
✔ Read climate: tmax, K → C, daily, 1981-01-01 – 2023-12-31 (ERA5-Land)  [1.1s]
✔ Extracted 312 points × 15,706 days                                   [8.4s]
✔ Thresholds above 30, 35 C; spells ≥ 2 days                           [2.0s]
ℹ Window summaries need ~12.4 GB; budget 8 GB. Splitting 312 locations into 2 chunks.
✔ Window summaries: 1,234 households × 2 lags                          [1m 20s]
✔ Done: 1,234 × 2 rows, 14 indicators                                  [1m 32s]
```

  Elapsed time per step is also stored in `meta$history`, so
  `c4e_history()` shows where a run spent its time. `verbose = FALSE` (or
  `options(climatic4economist.verbose = FALSE)`) silences steps, bars and
  defaults messages together; warnings and errors are never silenced.
- **Inside heavy steps.** Chunked work shows the bar above; unchunked long
  loops (per-layer extraction, per-scale SPI) show a `cli_progress_bar()` when
  they run longer than two seconds (`cli`'s default `show_after`), so short
  runs stay quiet.

## 15. Shareable plan and progress log

Git is the sharing mechanism: plan and log live in the repo on branch `v2`,
pushed at the end of every session, so a session on another laptop (or a
fresh Claude session here) resumes from `git pull` with no conversation
history. Three files, all created in chunk 0:

### `dev/PLAN_v2.md`

This document, verbatim. It changes only by deliberate edits recorded in the
log's decisions table (a chunk that deviates from the plan updates both). `dev/`
is in `.Rbuildignore`, so it never ships in the built package.

### `dev/LOG_v2.md`

The handoff document. A status table at the top, then one entry per session,
newest first. Template:

```markdown
# climatic4economist v2 — progress log

## Status (update at the end of every session)

| # | Chunk | State | PR | Started | Done | Notes |
|---|---|---|---|---|---|---|
| 0 | Scaffold | ✔ done | #12 | 2026-10-17 | 2026-10-17 | |
| 1 | c4e_tbl class | 🔄 in progress | #13 | 2026-10-18 | | `names<-` method still failing on grouped_df |
| 2 | Defaults, messages, history, chunking | ☐ | | | | |
| … | | | | | | |

**Current chunk:** 1 — `R/c4e_tbl.R`; tests in `test-c4e-tbl.R` 14/17 passing.
**Next steps (in order):**
1. fix `names<-.c4e_tbl` when x is a grouped_df (see entry 2026-10-18)
2. add vctrs `vec_ptype2` methods and the `bind_rows` test
3. open PR #13 for review, then start chunk 2
**Blocked / needs Jan:** none.

## Decisions and deviations from the plan

| Date | Section | Decision | Why |
|---|---|---|---|
| 2026-10-18 | §1 | attribute named `c4e_meta`, not `meta` | avoids clash with tibble internals |

## Session entries

### 2026-10-18 — chunk 1 (laptop: FAO-Win, session: Claude)
- Did: constructor, restore, `[`, `dplyr_reconstruct`, print; 14 tests.
- Found: `grouped_df` puts its class first, so `names<-` loses our attribute.
- Tests: `devtools::test()` 14 pass / 3 fail (all in `test-c4e-tbl.R`, names<-).
- Next: see Next steps above.
- Commits: `a1b2c3d`, `d4e5f6a` on `v2`.
```

Rules for the log:

- **Start of session:** read the status table and "Next steps"; `git pull`
  `v2`; run `devtools::test()` to confirm the recorded state before touching
  anything. If the recorded and actual states differ, the first log line of
  the session says so.
- **End of session:** update the status row, rewrite "Current chunk / Next
  steps / Blocked", add a dated entry (did / found / tests / next / commits),
  commit the log with the code, push. A session that ends mid-chunk still
  pushes, so the other laptop can pick it up; work in progress goes on `v2`
  directly or on a `v2-chunk-N` branch named in the log.
- **Deviations** from `PLAN_v2.md` go in the decisions table with the reason,
  and the plan is edited in the same commit.
- "Blocked / needs Jan" is the only place a question for the maintainer waits;
  the wrap-up of a session repeats it.

### Repo-root `CLAUDE.md`

A short project file so every Claude session in this repo, on any machine,
follows the same rules without being told (the OneDrive-level `CLAUDE.md`
stays untouched; the repo-level one is read in addition and scopes the
package work):

```markdown
# climatic4economist — package repo

- This is an R package under active v2 development on branch `v2`. Write
  access covers `R/`, `tests/`, `man/`, `vignettes/`, `inst/`, `dev/`,
  `DESCRIPTION`, `NAMESPACE`, `NEWS.md`, `README.md`. Raw data outside the
  repo stays read-only; `inst/extdata/` holds only synthetic fixtures.
- Start every session by reading `dev/LOG_v2.md` (status + next steps), then
  the relevant section of `dev/PLAN_v2.md`. End every session by updating the
  log and pushing (see the log's rules).
- One chunk per PR into `v2`; `main` changes only at release.
- Style: tidyverse, native pipe, explicit namespacing, `cli` for messages,
  testthat 3e; every exported function has tests on the synthetic fixtures.
- Verify with `devtools::test()` before claiming a step done; `devtools::check()`
  at the end of chunks 13–17.
```

Creating this file touches the repo root, which the OneDrive-level rules
reserve for explicit requests; approval of this plan is that request.

## Verification

- `devtools::test()` green after each phase; `devtools::check()` with no
  ERROR/WARNING at phase 6.
- Pipeline test: synthetic daily raster (3 years, 2 variables) + 20 fake
  households → the chain in §7.1 runs with context arguments given only once;
  `c4e_meta()` of the final table lists every indicator with its threshold, unit,
  min_spell and window; `write_stata()` output read back with `haven` shows the
  labels.
- Attribute survival test: `mutate`, `filter`, `select`, `rename`, `left_join`,
  `bind_rows`, `group_by |> summarise`, `[`, `saveRDS/readRDS` keep the meta;
  `pivot_longer` inside package functions re-wraps it.
- Deprecation test: every v1 name still runs its old example with a single
  `lifecycle` warning and returns the same values as the v2 call.
- Wrappers: `weather_extremes(preset = "heat")` on the synthetic data equals the
  step-by-step chain column for column; `c4e_history()` of its output, evaluated,
  reproduces it; every preset runs against a matching synthetic raster and errors
  cleanly against a mismatched one (wrong variable or unit).
- Memory and progress: with `options(climatic4economist.mem_limit = "1MB")`
  the pipeline test still returns tables identical to the unrestricted run,
  and `expect_snapshot()` captures the split message and the per-step lines of
  each wrapper; `verbose = FALSE` produces no output; `dry_run = TRUE` returns
  without computing and prints the estimates.
- Grid: `grid_indicators(preset = "heat", period = "year")` on the synthetic
  raster equals the yearly sum of `calc_threshold_day()` on the table extracted
  at the same cells; the same numbers come out with and without `filename =`
  (block-wise vs in-memory path); `grid_controls(radius_m = 5000)` equals a
  hand-computed `focal()` sum over `cellSize()`; `terra::metags()` of the
  output lists the registry and `extract_by_coord()` restores it.
- Performance: `inst/bench/` runs on 1k / 10k / 100k synthetic units before
  and after phase 3; the matrix kernel must match the v1 long-format results
  exactly (`expect_equal` on the full indicator tables) and the 10k case must
  not be slower on any step.
- §9 checks: `prepare_coord()` gives the same `ID` for the same coordinates
  across two differently ordered subsets; a window with fewer days than
  `min_coverage` returns `NA`, not 0; a 0–360 raster extracts the same values
  as its rotated copy; `c4e_history()` of the pipeline test re-runs to an
  identical table.
- Render the five canonical vignettes with `quarto render` from the synthetic
  data; the country vignettes are checked by `eval: false` review only, since
  their data is outside the repo.
