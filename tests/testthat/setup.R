# Run before the tests: the session's groundplotqc.lang and groundplotqc.severity are cleared
# for the whole run and put back after it, so a value set in the session can't change a
# test's result (D14.6, D14.9). A test that needs an option sets it with withr.
withr::local_options(
  groundplotqc.lang = NULL, groundplotqc.severity = NULL,
  .local_envir = testthat::teardown_env()
)
