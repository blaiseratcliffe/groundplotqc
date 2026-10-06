# Tests for MAGPlot 2.0's compiled specification (plan 3.6, 4.4; D12.20, D12.36).

test_that("magp_spec() is a gpq_spec whose pre-flight doesn't stop (D12.39)", {
  spec <- magp_spec()
  expect_s3_class(spec, "gpq_spec")
  # The fixed set still warns; the warning is muffled so only a stop fails the test.
  results <- withCallingHandlers(
    gpq_preflight(spec),
    gpq_preflight_warning = function(w) invokeRestart("muffleWarning")
  )
  # An empty table would pass the next line vacuously.
  expect_gt(nrow(results), 0L)
  expect_false(any(results$outcome == "stop"))
})
