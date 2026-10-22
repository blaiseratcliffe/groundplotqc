# Run before the tests: the session's groundplotqc options (lang, severity, M4a's
# example_cap, column_level_min_n, raw_na_policy, rogue_codes and unknown_codes, and M5's
# coordinate_attributes, code_set_max_values, code_set_skip_attributes,
# code_set_skip_pattern, design_geometry_table, design_geometry_by, design_geometry_columns,
# subplot_label_table, subplot_label_columns, design_signature_table, design_signature_by
# and design_signature_columns) are cleared for the whole run and put back after it, so a
# value set in the session can't change a test's result (D14.6, D14.9). A test that needs an
# option sets it with withr.
withr::local_options(
  groundplotqc.lang = NULL, groundplotqc.severity = NULL, groundplotqc.example_cap = NULL,
  groundplotqc.column_level_min_n = NULL, groundplotqc.raw_na_policy = NULL,
  groundplotqc.rogue_codes = NULL, groundplotqc.unknown_codes = NULL,
  groundplotqc.coordinate_attributes = NULL, groundplotqc.code_set_max_values = NULL,
  groundplotqc.code_set_skip_attributes = NULL, groundplotqc.code_set_skip_pattern = NULL,
  groundplotqc.design_geometry_table = NULL, groundplotqc.design_geometry_by = NULL,
  groundplotqc.design_geometry_columns = NULL, groundplotqc.subplot_label_table = NULL,
  groundplotqc.subplot_label_columns = NULL, groundplotqc.design_signature_table = NULL,
  groundplotqc.design_signature_by = NULL, groundplotqc.design_signature_columns = NULL,
  .local_envir = testthat::teardown_env()
)
