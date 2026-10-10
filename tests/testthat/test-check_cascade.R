# Tests for the cascade: the cells that failed Layers 1 to 3 and the records a later check
# mustn't evaluate (plan 6.2; D2.10, D7.3).

# Issues from Layers 1 to 4 on one table, two columns: cell (a, row 1) fails Layers 2 and
# 1, (a, row 2) Layer 3, (b, row 1) Layer 2; a column-level issue, a not_evaluated one and a
# Layer 4 one, which mask nothing.
cascade_issues <- function() {
  data.table::data.table(
    issue_id = 1:7, table_name = c("t", "t", "t", "t", "t", "t", "u"),
    column_name = c("a", "a", "a", "b", "b", "a", "a"),
    row_number = c(1L, 1L, 2L, 1L, NA, 3L, 1L),
    layer = c(2L, 1L, 3L, 2L, 1L, 2L, 4L),
    level = c("record", "record", "record", "record", "column", "record", "record"),
    status = c("open", "open", "open", "open", "open", "not_evaluated", "open")
  )
}

test_that("failed_cells() lists each failed cell once, its root the earliest failure", {
  cells <- failed_cells(cascade_issues())
  expect_equal(names(cells), c("table_name", "column_name", "row_number", "layer", "issue_id"))
  expect_equal(cells$column_name, c("a", "a", "b"))
  expect_equal(cells$row_number, c(1L, 2L, 1L))
  # Cell (a, 1) failed Layers 2 (issue 1) and 1 (issue 2): its root is issue 2.
  expect_equal(cells$issue_id, c(2L, 3L, 4L))
  expect_equal(nrow(failed_cells(cascade_issues(), layers = 4L)), 1L)
  expect_equal(nrow(failed_cells(cascade_issues()[0L])), 0L)
})

test_that("mark_not_evaluated() masks the records whose cells failed an earlier layer", {
  cells <- failed_cells(cascade_issues())
  # A Layer 3 check reading a: row 1, root issue 2; row 2 failed Layer 3, not an earlier one.
  masked <- mark_not_evaluated(cells, "t", "a", layer = 3L)
  expect_equal(masked$row_number, 1L)
  expect_equal(masked$root_issue_id, 2L)
  # A Layer 4 check reading a and b: rows 1 and 2, row 1's root the lowest layer's issue.
  masked <- mark_not_evaluated(cells, "t", c("a", "b"), layer = 4L)
  expect_equal(masked$row_number, c(1L, 2L))
  expect_equal(masked$root_issue_id, c(2L, 3L))
  # Only the check's own rows, columns and table.
  expect_equal(mark_not_evaluated(cells, "t", c("a", "b"), 4L, rows = 2:5)$row_number, 2L)
  expect_equal(nrow(mark_not_evaluated(cells, "t", "c", 4L)), 0L)
  expect_equal(nrow(mark_not_evaluated(cells, "u", "a", 4L)), 0L)
  # A Layer 1 check is masked by nothing (no earlier layer).
  expect_equal(nrow(mark_not_evaluated(cells, "t", c("a", "b"), 1L)), 0L)
  expect_named(mark_not_evaluated(cells, "t", "a", 2L), c("row_number", "root_issue_id"))
})
