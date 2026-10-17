# Run before the tests: the session's groundplotqc options (lang, severity, M4a's
# example_cap, column_level_min_n, raw_na_policy, rogue_codes and unknown_codes, and M5's
# coordinate_attributes) are cleared for the whole run and put back after it, so a value set
# in the session can't change a test's result (D14.6, D14.9). A test that needs an option
# sets it with withr.
withr::local_options(
  groundplotqc.lang = NULL, groundplotqc.severity = NULL, groundplotqc.example_cap = NULL,
  groundplotqc.column_level_min_n = NULL, groundplotqc.raw_na_policy = NULL,
  groundplotqc.rogue_codes = NULL, groundplotqc.unknown_codes = NULL,
  groundplotqc.coordinate_attributes = NULL,
  .local_envir = testthat::teardown_env()
)
