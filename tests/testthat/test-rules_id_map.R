# Tests for the rule ID map (plan 9.6, 17.4; D4.2, D5.2, D9.20).

test_that("the shipped rule ID map reads clean with its four columns", {
  map <- rule_id_map()
  expect_equal(names(map), c("old_rule_id", "new_rule_id", "change", "package_version"))
  expect_true(all(vapply(map, is.character, logical(1))))
})

test_that("a valid map passes: renames and splits to registered rules", {
  registry <- data.table::data.table(rule_id = c("fk_orphan", "pk_a", "pk_b"))
  map <- data.table::data.table(
    old_rule_id = c("fk_missing_parent", "pk_bad", "pk_bad"),
    new_rule_id = c("fk_orphan", "pk_a", "pk_b"), change = c("rename", "split", "split"),
    package_version = "0.0.0.9002"
  )
  expect_no_error(validate_rule_id_map(map, registry))
  expect_no_error(validate_rule_id_map(map[0L], registry))
})

test_that("a map that breaks a rule stops, naming the rows' old rule IDs (D5.2)", {
  registry <- data.table::data.table(rule_id = c("fk_orphan", "pk_a", "pk_b"))
  row <- function(old, new, change = "rename") {
    data.table::data.table(
      old_rule_id = old, new_rule_id = new, change = change, package_version = "0.0.0.9002"
    )
  }
  check <- function(map) validate_rule_id_map(map, registry)
  expect_error(check(row("pk_a", "pk_b")), "names registered rules as old rule IDs: pk_a")
  expect_error(check(row("gone", "also_gone")), "unregistered rules as new rule IDs for: gone")
  expect_error(check(row("gone", NA)), "blank cells in the rows of: gone")
  expect_error(check(row("gone", "pk_a", "merge")), "other than rename or split for: gone")
  expect_error(check(rbind(row("gone", "pk_a"), row("gone", "pk_a"))), "repeats a row for: gone")
  expect_error(
    check(rbind(row("gone", "pk_a"), row("gone", "pk_b"))),
    "more than one rename row for: gone"
  )
  expect_error(check(row("gone", "pk_a", "split")), "a split with one row for: gone")
  expect_error(
    check(rbind(row("gone", "pk_a"), row("gone", "pk_b", "split"))),
    "mixes rename and split for: gone"
  )
  expect_error(check(row("gone", "pk_a")[, -4L]), "has the columns old_rule_id")
})

test_that("a new rule ID belongs to one old rule ID, once: no merge in effect (D9.20, D15.23)", {
  registry <- data.table::data.table(rule_id = c("fk_orphan", "pk_a", "pk_b"))
  row <- function(old, new, change = "rename", version = "0.0.0.9002") {
    data.table::data.table(
      old_rule_id = old, new_rule_id = new, change = change, package_version = version
    )
  }
  check <- function(map) validate_rule_id_map(map, registry)
  # Two old rule IDs renamed to one new rule ID; each stop names the rows' old rule IDs.
  expect_error(
    check(rbind(row("gone", "pk_a"), row("gone_too", "pk_a"), row("other", "pk_b"))),
    "gives one new rule ID to more than one old rule ID for: gone, gone_too"
  )
  # A split of one old rule ID naming one new rule ID twice, in two package versions.
  expect_error(
    check(rbind(
      row("gone", "pk_a", "split"), row("gone", "pk_a", "split", "0.0.0.9003"),
      row("gone", "pk_b", "split")
    )),
    "names one new rule ID more than once for: gone"
  )
  # A split to different new rule IDs, and renames to different ones, still pass.
  expect_no_error(check(rbind(row("gone", "pk_a", "split"), row("gone", "pk_b", "split"))))
  expect_no_error(check(rbind(row("gone", "pk_a"), row("gone_too", "pk_b"))))
})

test_that("rule_id_map() stops on a file missing, one not read cleanly, one that breaks a rule", {
  # system.file() stands in for the package's file, for the test only. Under load_all() the
  # package calls pkgload's own system.file(), found among its imports; an installed
  # package, as R CMD check runs it, calls base's.
  imports <- parent.env(environment(rule_id_map))
  in_imports <- exists("system.file", envir = imports, inherits = FALSE)
  mock_path <- function(path, env = parent.frame()) {
    stand_in <- function(...) path
    if (in_imports) {
      testthat::local_mocked_bindings(system.file = stand_in, .env = env)
    } else {
      testthat::local_mocked_bindings(system.file = stand_in, .package = "base", .env = env)
    }
  }
  mock_path("")
  expect_error(rule_id_map(), "The package has no rule_id_map.csv.")
  file <- withr::local_tempfile(fileext = ".csv")
  mock_path(file)
  header <- "old_rule_id,new_rule_id,change,package_version"
  writeLines(c(header, "gone,pk_a,rename,0.0.0.9002,extra"), file)
  expect_error(rule_id_map(), "The package's rule_id_map.csv doesn't read cleanly.")
  invalid <- c(charToRaw(paste0(header, "\ngone,pk")), as.raw(0x97), charToRaw(",rename,1\n"))
  writeBin(invalid, file)
  expect_error(rule_id_map(), "The package's rule_id_map.csv doesn't read cleanly.")
  # A file that reads but breaks the validator: an old rule ID that is registered.
  registered <- rule_registry()$rule_id
  row <- paste(registered[[1L]], registered[[2L]], "rename", "0.0.0.9002", sep = ",")
  writeLines(c(header, row), file)
  expect_error(rule_id_map(), paste("names registered rules as old rule IDs:", registered[[1L]]))
})
