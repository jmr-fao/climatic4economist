# climatic4economist v2 — progress log

Handoff document for v2 development. Read this first in every session, on any
laptop; the design is in `dev/PLAN_v2.md` (section numbers below refer to it).

## Status (update at the end of every session)

| # | Chunk | State | PR | Started | Done | Notes |
|---|---|---|---|---|---|---|
| 0 | Scaffold | 🔄 in progress | | 2026-10-16 | | committed locally on `v2`, not pushed yet |
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

**Current chunk:** 0 — scaffold.

**Next steps (in order):**
1. Jan: push branch `v2` (`git push -u origin v2`) so other laptops can resume.
2. Mark chunk 0 done once pushed; the CI workflow runs on the first push.
3. Start chunk 1 (`R/c4e_tbl.R`, plan §1), with `test-c4e-tbl.R` first.

**Blocked / needs Jan:**
- Push of branch `v2` (outward-facing, left to Jan).
- `roxygen2`, `devtools`, `usethis` and `rcmdcheck` are not installed in the
  R 4.5 library on the FAO-Win laptop. Tests run through
  `testthat::test_local()` (pkgload is installed), but `NAMESPACE` and `man/`
  cannot be regenerated until `roxygen2` is installed, which chunk 1 needs.
  Install with `install.packages(c("roxygen2", "devtools"))`.

## Decisions and deviations from the plan

| Date | Section | Decision | Why |
|---|---|---|---|
| 2026-10-16 | §8 | Chunk 0 commits locally; pushing is left to Jan | pushing a branch is outward-facing |
| 2026-10-16 | §2, §13 | `read_climate()` must not rely on `terra::varnames()` for the variable: on read, terra replaces it with the file name (`tmax_daily`). Store the variable in `terra::metags()` and fall back to an explicit argument. `time()` and `units()` do round-trip through GeoTIFF. | found while building fixtures |
| 2026-10-16 | §8 chunk 0 | The fixture generator lives in `dev/make_fixtures.R` (build-ignored), not in a `data-raw/` folder | keeps all v2 development material in one place |

## How to run things on a machine without R on the PATH

```bash
"/c/Program Files/R/R-4.5.0/bin/Rscript.exe" -e 'testthat::test_local()'
```

Fixtures are rebuilt (identically, seeded) with:

```bash
"/c/Program Files/R/R-4.5.0/bin/Rscript.exe" dev/make_fixtures.R
```

## Session entries

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
