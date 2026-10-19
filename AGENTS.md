# climatic4economist — package repo

- This is an R package under active v2 development on branch `v2`. Write
  access covers `R/`, `tests/`, `man/`, `vignettes/`, `inst/`, `dev/`,
  `.github/`, `DESCRIPTION`, `NAMESPACE`, `NEWS.md`, `README.md`. Raw data
  outside the repo stays read-only; `inst/extdata/` holds only synthetic
  fixtures.
- Start every session by reading `dev/LOG_v2.md` (status and next steps),
  then the relevant section of `dev/PLAN_v2.md`. Run the test suite to confirm
  the recorded state before changing anything.
- End every session by updating the log: status row, current chunk, next
  steps, blocked items, a dated entry (did / found / tests / next / commits).
  Commit the log with the code. Ask before pushing.
- Deviations from `dev/PLAN_v2.md` go in the log's decisions table, and the
  plan is edited in the same commit.
- One chunk per PR into `v2`; `main` changes only at release.
- Style: tidyverse, native pipe, explicit namespacing, `cli` for messages,
  testthat 3rd edition; every exported function has tests on the synthetic
  fixtures in `inst/extdata/`.
- Verify before claiming a step done:
  `"/c/Program Files/R/R-4.5.0/bin/Rscript.exe" -e 'testthat::test_local()'`
  on the FAO laptop where R is not on the PATH.
