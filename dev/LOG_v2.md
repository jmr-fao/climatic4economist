# climatic4economist v2 — progress log

Handoff document for v2 development. Read this first in every session, on any
laptop; the design is in `dev/PLAN_v2.md` (section numbers below refer to it).

## Status (update at the end of every session)

| # | Chunk | State | PR | Started | Done | Notes |
|---|---|---|---|---|---|---|
| 0 | Scaffold | ✔ done | | 2026-10-16 | 2026-10-19 | pushed; CI fixed on 2026-10-19 (examples, `withr`) |
| 1 | `c4e_tbl` class | ☐ | | | | |
| 2 | Defaults, messages, history, chunking | ☐ | | | | |
| 3 | Units | ☐ | | | | |
| 4 | Raster readers | ☐ | | | | |
| 5 | Extraction | ☐ | | | | |
| 6 | Matrix kernel | ☐ | | | | |
| 7 | Daily indicators | ☐ | | | | |
| 8 | Standardised indices and parameters | ☐ | | | | |
| 9 | Survey side | ☐ | | | | |
| 10 | Export and inspection | ☐ | | | | |
| 11 | Wrappers and presets | ☐ | | | | |
| 12 | Grid methods and wrappers | ☐ | | | | |
| 13 | Deprecations and removals | ☐ | | | | |
| 14 | Package hygiene | ☐ | | | | |
| 15 | Quick start and canonical vignettes | ☐ | | | | |
| 16 | Country case studies | ☐ | | | | |
| 17 | Release 2.0.0 | ☐ | | | | |

States: ☐ not started · 🔄 in progress · 👀 in review (PR open) · ✔ done · ⛔ blocked

**Current chunk:** none in progress; chunk 1 is next.

**Next steps (in order):**
1. Jan: push the CI fix on `v2` and confirm R-CMD-check goes green on
   GitHub (it now fails on any ERROR or WARNING).
2. Start chunk 1 (`R/c4e_tbl.R`, plan §1), with `test-c4e-tbl.R` first.

**Blocked / needs Jan:**
- Push of the CI-fix commit on `v2` (outward-facing, left to Jan).
- Pull request 3 merges `v2` into `main`. The plan merges `main` only at
  release (chunk 17). Keep it open as a draft tracking PR, or close it; don't
  merge it yet.

## Decisions and deviations from the plan

| Date | Section | Decision | Why |
|---|---|---|---|
| 2026-10-16 | §8 | Chunk 0 commits locally; pushing is left to Jan | pushing a branch is outward-facing |
| 2026-10-16 | §2, §13 | `read_climate()` must not rely on `terra::varnames()` for the variable: on read, terra replaces it with the file name (`tmax_daily`). Store the variable in `terra::metags()` and fall back to an explicit argument. `time()` and `units()` do round-trip through GeoTIFF. | found while building fixtures |
| 2026-10-16 | §8 chunk 0 | The fixture generator lives in `dev/make_fixtures.R` (build-ignored), not in a `data-raw/` folder | keeps all v2 development material in one place |
| 2026-10-16 | §8 chunk 0 | `.gitignore` keeps ignoring every `.tif`/`.gpkg` but re-includes `inst/extdata/*.tif` and `*.gpkg` | protect real data while tracking the synthetic fixtures |
| 2026-10-19 | §8 chunks 0, 14 | R-CMD-check is a real gate from now on, not "allowed to fail until chunk 14". Every chunk must leave 0 ERRORs and 0 WARNINGs; NOTEs stay for chunk 14. | Jan asked for the check to pass for real |
| 2026-10-19 | §3 | `merge_by_common()` list form fixed now (reduce over the list) instead of in chunk 9 | its example exposed the bug |

### Open R CMD check NOTEs (for chunk 14)

- Title not in title case; Description not complete sentences; maintainer email `fao.com`.
- Tarball is about 19.8 MB: vignette HTML and images are not build-ignored.
- `cli`, `lifecycle`, `vctrs` declared but not yet used (go away in chunks 1–2).
- Many "no visible binding for global variable" notes (NSE columns) and unqualified sf/sp/raster calls in `ave_*` (removed in chunk 13).
- `AGENTS.md` and `figures/` at top level (`AGENTS.md` is now build-ignored).
- `extract_cell_by_poly()` example takes about 6 s because of the `raster::brick()` conversion (removed in chunk 5).
- FAO GAUL URL in `read_GAUL.Rd` answers 403 to the checker.

## How to run things on a machine without R on the PATH

```bash
"/c/Program Files/R/R-4.5.0/bin/Rscript.exe" -e 'testthat::test_local()'
```

Fixtures are rebuilt (identically, seeded) with:

```bash
"/c/Program Files/R/R-4.5.0/bin/Rscript.exe" dev/make_fixtures.R
```

To reproduce the GitHub check locally (same arguments as CI):

```bash
"/c/Program Files/R/R-4.5.0/bin/Rscript.exe" -e 'rcmdcheck::rcmdcheck(args = c("--no-manual", "--as-cran", "--ignore-vignettes"), build_args = c("--no-manual", "--no-build-vignettes"), error_on = "warning")'
```

## Session entries

### 2026-10-19 — CI fix after chunk 0 (laptop: FAO-Win, session: Claude)
- Did: installed `roxygen2` 8.1.1 and `devtools` 2.5.2. Fixed the 16 help
  pages whose examples failed: most referenced objects that did not exist
  and now run on the bundled fixtures; `find_spell()` used 0/1 instead of
  TRUE/FALSE flags; `extract_by_poly()` called `ncell()` unqualified;
  `ave_dist_2shp()` is wrapped in `\dontrun` (removed in chunk 13);
  `spi_wide()`/`spei_wide()` are internal, so they lost their help pages.
  Fixed `merge_by_common()` for a list of data frames. Corrected the
  parameter docs of `extract_by_coord()`. Declared `withr` in Suggests,
  build-ignored `AGENTS.md`, made the CI job blocking, `actions/checkout@v5`.
- Found: PR 3 (`v2` → `main`) triggered the runs; the failure was R CMD
  check stopping at the first broken example, plus the `withr` WARNING.
  GitHub itself was fine.
- Tests: 26 files, 336 tests, 679 expectations, 0 failures. Local
  `rcmdcheck` with CI's arguments: 0 errors, 0 warnings, 8 notes (listed
  above).
- Next: see Next steps above.

### 2026-10-16 — chunk 0 (laptop: FAO-Win, session: Claude)
- Did: designed v2 with Jan (plan approved after seven rounds of additions:
  wrappers, presets, grid computation, memory chunking, progress, this log).
  Created branch `v2`; added `dev/PLAN_v2.md`, this log, a repo-level
  `CLAUDE.md`, build-ignore entries for `dev/`, `CLAUDE.md`, `.github/`;
  version `2.0.0.9000`; `cli`, `lifecycle`, `vctrs` in Imports and `haven` in
  Suggests; `NEWS.md`; `.github/workflows/R-CMD-check.yaml` (non-blocking
  until chunk 14); fixtures built by `dev/make_fixtures.R` into
  `inst/extdata/` (daily tmax in K and precipitation in mm/day for 2021–2023,
  monthly precipitation for 1984–2023, 20 households, 3 divisions; about
  2 MB); `tests/testthat/helper-synthetic.R` and `test-fixtures.R`.
- Found: `clock` was missing from the R 4.5 user library, so it was installed
  from CRAN. `terra::varnames()` does not survive a GeoTIFF round trip (see
  decisions). R 4.4.x installs on this laptop have no package library, so use
  R 4.5.0.
- Tests: 26 files, 334 tests, 675 expectations passed, 0 failures. One
  pre-existing warning comes from the SPEI fit in `test-compute-spei.R`.
- Next: see Next steps above.
- Commits on `v2`: `8d8e5b1` (scaffold), `4621557` (track fixtures), and
  the commit that adds this line.
