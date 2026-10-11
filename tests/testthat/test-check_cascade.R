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

test_that("a root is the lowest layer, then the lowest issue ID: on a cell, a row and a table", {
  # Issues listed with their IDs out of order. On table t: cell (a, 1) has two Layer 2 issues
  # (9 listed before 4); row 2 has a Layer 1 issue (8) in a and a Layer 2 one (3) in b; row 3
  # has Layer 2 issues in a (7) and b (5). Table u has a Layer 2 issue (10) on the same column
  # and row as (t, a, 1). A column-level Layer 1 issue (11) on t carries a row number.
  issues <- data.table::data.table(
    issue_id = c(9L, 4L, 8L, 3L, 7L, 5L, 10L, 11L),
    table_name = c("t", "t", "t", "t", "t", "t", "u", "t"),
    column_name = c("a", "a", "a", "b", "a", "b", "a", "b"),
    row_number = c(1L, 1L, 2L, 2L, 3L, 3L, 1L, 4L),
    layer = c(2L, 2L, 1L, 2L, 2L, 2L, 2L, 1L),
    level = c(rep("record", 7L), "column"),
    status = "open"
  )
  issues_before <- data.table::copy(issues)
  cells <- failed_cells(issues)
  cells_before <- data.table::copy(cells)
  # One row per cell: (t, a, 1) keeps issue 4 of its two, (u, a, 1) is a cell of its own, and
  # the column-level issue (row number 4) is no cell.
  expect_equal(cells$table_name, c("t", "t", "t", "t", "t", "u"))
  expect_equal(cells$column_name, c("a", "a", "a", "b", "b", "a"))
  expect_equal(cells$row_number, c(1L, 2L, 3L, 2L, 3L, 1L))
  expect_equal(cells$issue_id, c(4L, 8L, 7L, 3L, 5L, 10L))
  expect_equal(cells$layer, c(2L, 1L, 2L, 2L, 2L, 2L))
  # A table's check sees its own cells only.
  expect_equal(mark_not_evaluated(cells, "u", "a", 3L)$root_issue_id, 10L)
  expect_equal(mark_not_evaluated(cells, "t", "a", 3L, rows = 1L)$root_issue_id, 4L)
  # A row's root across columns. Row 1: the lowest ID of the two Layer 2 issues. Row 2: Layer
  # 1's issue 8 over Layer 2's issue 3. Row 3: issue 5 of b over issue 7 of a, both Layer 2.
  masked <- mark_not_evaluated(cells, "t", c("a", "b"), layer = 3L)
  expect_equal(masked$row_number, 1:3)
  expect_equal(masked$root_issue_id, c(4L, 8L, 5L))
  # A check of Layer 2 is masked by Layer 1's issue only.
  masked <- mark_not_evaluated(cells, "t", c("a", "b"), layer = 2L)
  expect_equal(masked$row_number, 2L)
  expect_equal(masked$root_issue_id, 8L)
  # Neither the issues nor the failed cells given were changed.
  mark_not_evaluated(cells, "t", c("a", "b"), layer = 3L, rows = 2:3)
  expect_identical(issues, issues_before)
  expect_identical(cells, cells_before)
})

test_that("only open record-level issues with a row number make a failed cell", {
  issues <- cascade_issues()
  # The column-level Layer 1 issue (5) given a row number is still no cell: (b, 1) keeps its
  # Layer 2 issue 4 as its root.
  data.table::set(issues, i = 5L, j = "row_number", value = 1L)
  cells <- failed_cells(issues)
  expect_equal(cells$issue_id, c(2L, 3L, 4L))
  # A record-level issue that isn't open, and one with no row number, are no cells either.
  extra <- data.table::data.table(
    issue_id = 8:9, table_name = "t", column_name = "c", row_number = c(1L, NA), layer = 1L,
    level = "record", status = c("not_evaluated", "open")
  )
  expect_false("c" %in% failed_cells(rbind(issues, extra))$column_name)
})

test_that("mark_not_evaluated() takes one table name", {
  cells <- failed_cells(cascade_issues())
  expect_error(mark_not_evaluated(cells, c("t", "u"), "a", 4L), "`table_name` must be one table")
  expect_error(mark_not_evaluated(cells, character(), "a", 4L), "`table_name` must be one table")
  expect_error(mark_not_evaluated(cells, NA_character_, "a", 4L), "`table_name` must be one table")
  expect_error(mark_not_evaluated(cells, 1L, "a", 4L), "`table_name` must be one table")
  expect_equal(mark_not_evaluated(cells, "t", "a", 3L)$row_number, 1L)
})
