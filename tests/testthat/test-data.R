# Tests for the clean base, the generic fixtures, the fixture marker and the answer sheet
# (plan 18.3; D5.11, D8.21, D15.10), and for magp_example, the example data built from the
# clean base (18.1, 21 M3b).

# Every path of foreign keys from each row of `table` to each ancestor table, as that
# ancestor's PK value: a list by ancestor of a matrix, one column per path.
key_paths <- function(tables, spec, table) {
  links <- spec$keys[spec$keys$key_type == "FK"]
  primary <- spec$keys[spec$keys$key_type == "PK"]
  pk_of <- function(name) primary$attribute_name[primary$table_name == name]
  # Each path's values from `rows` of `name`, named by the ancestor the path reaches.
  walk <- function(name, rows) {
    reached <- list()
    for (k in which(links$table_name == name)) {
      parent <- links$reference_table[[k]]
      values <- tables[[name]][[links$attribute_name[[k]]]][rows]
      parent_rows <- match(values, tables[[parent]][[pk_of(parent)]])
      reached <- c(
        reached, structure(list(as.character(values)), names = parent), walk(parent, parent_rows)
      )
    }
    reached
  }
  paths <- walk(table, seq_len(nrow(tables[[table]])))
  ancestors <- factor(names(paths), unique(names(paths)))
  lapply(split(paths, ancestors), function(x) do.call(cbind, unname(x)))
}

# The properties every clean base has, whatever its spec.
expect_clean_base <- function(tables, spec) {
  a <- spec$attributes
  expect_equal(names(tables), unique(a$table_name))
  date_formats <- spec$type_map$date_format[match(a$data_type, spec$type_map$data_type)]
  for (name in names(tables)) {
    table <- tables[[name]]
    columns <- a[a$table_name == name]
    expect_equal(names(table), columns$attribute_name, label = name)
    expect_equal(
      unname(vapply(table, function(x) class(x)[[1L]], "")),
      fifelse(columns$r_class == "double", "numeric", columns$r_class),
      label = name
    )
    expect_false(anyNA(table), label = name)
    expect_true(isTRUE(attr(table, "gpq_fixture")), label = name)
    for (k in seq_len(nrow(columns))) {
      column <- columns$attribute_name[[k]]
      value <- as.character(table[[column]])
      where <- paste0(name, ".", column)
      expect_false(any(value != trimws(value) | !nzchar(value)), label = where)
      coded <- spec$codes$table_name == name & spec$codes$attribute_name == column
      codes <- spec$codes$code[coded]
      if (length(codes) > 0L) {
        expect_true(all(value %in% codes), label = where)
      }
      format <- date_formats[a$table_name == name][[k]]
      if (!is.na(format)) {
        expect_false(anyNA(as.Date(value, format)), label = where)
      }
      expect_false(any(value %in% spec$sentinels$value), label = where)
    }
  }
  # Primary keys are unique; each foreign key points to its parent's row; and every two key
  # paths from a record to one ancestor reach the same row.
  keys <- spec$keys
  for (k in which(keys$key_type == "PK")) {
    expect_false(anyDuplicated(tables[[keys$table_name[[k]]]][[keys$attribute_name[[k]]]]) > 0L)
  }
  for (k in which(keys$key_type == "FK" & keys$reference_table %in% names(tables))) {
    values <- tables[[keys$table_name[[k]]]][[keys$attribute_name[[k]]]]
    parent <- tables[[keys$reference_table[[k]]]][[keys$reference_attribute[[k]]]]
    where <- paste0(keys$table_name[[k]], ".", keys$attribute_name[[k]])
    expect_true(all(values %in% parent), label = where)
  }
  for (name in names(tables)) {
    for (paths in key_paths(tables, spec, name)) {
      expect_true(all(paths == paths[, 1L]), label = name)
    }
  }
  expect_true(isTRUE(attr(tables, "gpq_fixture")))
}

test_that("MAGPlot's clean base has every DD table and column, of its R class (21 M3b gate)", {
  spec <- magp_spec()
  tables <- fx_magp_clean()
  expect_equal(length(tables), 17L)
  expect_clean_base(tables, spec)
  # Two contributors, each with two datasets.
  expect_equal(nrow(tables$magp_contributors), 2L)
  expect_equal(as.vector(table(tables$magp_datasets$magp_contributor_id)), c(2L, 2L))
})

test_that("the clean base's spec passes pre-flight with MAGPlot's rule set (21 M3b gate)", {
  expect_no_error(suppressWarnings(gpq_preflight(magp_spec(), rules = magp_rules())))
})

test_that("the toy specs build clean the same way: the generic fixtures (18.3)", {
  expect_clean_base(fx_fish_clean(), fx_fish_spec())
  expect_equal(vapply(fx_fish_clean(), nrow, 1L), c(stations = 2L, hauls = 4L, catches = 8L))
  expect_clean_base(fx_clean_tables(fx_forest_spec()), fx_forest_spec())
})

test_that("the clean base refuses a spec it can't build", {
  spec <- fx_fish_spec()
  looped <- spec
  looped$keys <- rbind(spec$keys, data.table::data.table(
    table_name = "stations", attribute_name = "station_name", key_type = "FK", key_part = 1L,
    reference_table = "catches", reference_attribute = "catch_id"
  ))
  expect_error(fx_clean_tables(looped), "go round in a circle")
  twice <- spec
  twice$keys <- rbind(spec$keys, spec$keys[spec$keys$key_type == "PK"][1L])
  expect_error(fx_clean_tables(twice), "single-column PK")
})

test_that("the fixture guard refuses tables without the marker (18.3, D8.21)", {
  tables <- fx_fish_clean()
  expect_invisible(fx_require_fixture(tables))
  expect_error(fx_require_fixture(unclass(list(a = 1))), "no gpq_fixture attribute")
  plain <- tables
  attr(plain, "gpq_fixture") <- NULL
  expect_error(fx_require_fixture(plain), "Test helpers take fixtures only")
})

test_that("the answer sheet and the defect blocks match, every rule covered (18.3)", {
  sheet <- fx_answer_sheet()
  blocks <- fx_defect_blocks()
  expect_equal(names(sheet), c(
    "defect_id", "table_name", "column_name", "record_pk", "edit", "rule_id",
    "cascade_rule_ids", "witness"
  ))
  expect_setequal(sheet$defect_id, names(blocks))
  expect_true(all(vapply(blocks, is.function, logical(1))))
  registry <- rule_registry()
  data_rules <- registry$rule_id[registry$check_type != "preflight"]
  expect_true(all(sheet$rule_id %in% data_rules))
  # Every data rule has a defect block; pre-flight checks have their planted spec.
  expect_equal(setdiff(data_rules, sheet$rule_id), character())
})

test_that("magp_example is the clean base, marked as synthetic (D15.10)", {
  # Compared column by column: a saved data.table comes back without its spare slots.
  expect_equal(lapply(magp_example, as.list), lapply(fx_magp_clean(), as.list))
  expect_true(isTRUE(attr(magp_example, "gpq_fixture")))
  expect_true(all(vapply(magp_example, function(table) {
    data.table::is.data.table(table) && isTRUE(attr(table, "gpq_fixture"))
  }, logical(1))))
})
