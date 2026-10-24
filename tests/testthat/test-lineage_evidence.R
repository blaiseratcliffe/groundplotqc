# Tests for the evidence that settles issues whose class is depends (plan 3.6, 10.3 steps 2 to
# 5; D19.19 to D19.22, D19.58, D19.61). The issues are helper-classes.R's.

test_that("computed settles its table and column's issues, \"*\" any group value (D19.58 (1))", {
  issues <- fx_class_issues()
  computed <- data.frame(
    table_name = c("hauls", "catches", "hauls"),
    column_name = c("minutes", "species", "haul_id"), water_body = c("*", "LK", "*"),
    derivation = c("formula", "class", "id")
  )
  gpq_classify_issues(issues, NULL, evidence = list(computed = computed))
  # 1, 6 and 7 are LK's catches.species, 6 at column level; 2 is RV's; 4's class is fixed.
  expect_equal(issues$class_basis, c(
    "computed", "default", "computed", "default", "default", "computed", "computed", "default"
  ))
  expect_equal(issues$issue_class, c(
    "harmonization", "unclassified", "harmonization", "harmonization", "unclassified",
    "harmonization", "harmonization", "unclassified"
  ))
  # From a CSV file, as a data.frame.
  path <- withr::local_tempfile(fileext = ".csv")
  data.table::fwrite(computed, path)
  again <- fx_class_issues()
  gpq_classify_issues(again, NULL, evidence = list(computed = path))
  expect_identical(again$class_basis, issues$class_basis)
  # A table and column listed twice, with two derivations, settle an issue once.
  repeated <- rbind(computed, transform(computed[1L, ], derivation = "count"))
  both <- fx_class_issues()
  gpq_classify_issues(both, NULL, evidence = list(computed = repeated))
  expect_identical(both$class_basis, issues$class_basis)
  # A blank group cell is not "*" and matches no value (R37), nor an issue's blank group value
  # (R39).
  blank <- data.frame(
    table_name = "hauls", column_name = "minutes", water_body = NA_character_,
    derivation = "formula"
  )
  blank_issues <- fx_class_issues()
  gpq_classify_issues(blank_issues, NULL, evidence = list(computed = blank))
  expect_equal(blank_issues$class_basis[[3L]], "default")
  data.table::set(blank_issues, i = 3L, j = "water_body", value = NA_character_)
  gpq_classify_issues(blank_issues, NULL, evidence = list(computed = blank))
  expect_equal(blank_issues$class_basis[[3L]], "default")
})

test_that("filled settles a record's issue whose value is the value written (D19.58 (2))", {
  issues <- fx_class_issues()
  filled <- data.frame(
    table_name = c("hauls", "catches", "catches"), record_pk = c("H1", "C2", "*"),
    column_name = c("haul_date", "species", "species"),
    written_value = c("2020-13-01", "XX", NA)
  )
  gpq_classify_issues(issues, NULL, evidence = list(filled = filled))
  # 5's value is the one written; 2's isn't; the column-level 6 is never a cell's.
  expect_equal(issues$class_basis[c(2L, 5L, 6L)], c("default", "filled", "default"))
  expect_equal(issues$issue_class[[5L]], "harmonization")
  # computed comes first.
  computed <- data.frame(
    table_name = "hauls", column_name = "haul_date", water_body = "*", derivation = "formula"
  )
  gpq_classify_issues(issues, NULL, evidence = list(filled = filled, computed = computed))
  expect_equal(issues$class_basis[[5L]], "computed")
  # A blank value written matches no value, nor an issue's blank value (R39).
  blank <- data.frame(
    table_name = "hauls", record_pk = "H1", column_name = "haul_date",
    written_value = NA_character_
  )
  data.table::set(issues, i = 5L, j = "value", value = NA_character_)
  gpq_classify_issues(issues, NULL, evidence = list(filled = blank))
  expect_equal(issues$class_basis[[5L]], "default")
})

test_that("a code's raw value: valid gives lineage, a changed invalid one crosswalk (D19.61)", {
  issues <- fx_class_issues()
  lineage <- data.frame(
    table_name = "catches", record_pk = c("C1", "C3"), column_name = "species",
    raw_value = c("Pw", "bad"), raw_source = "catch.sp"
  )
  crosswalk <- data.frame(
    provider_code = c("Pw", "PW"), code = "NOP", column_name = "species",
    water_body = c("LK", "RV")
  )
  gpq_classify_issues(
    issues, NULL,
    evidence = list(lineage = lineage, crosswalk = crosswalk)
  )
  # 1: its raw value Pw is LK's provider code. 2: unlisted, its own value PW is RV's. 7: the
  # pipeline changed "bad", which isn't a code. 6 is column-level, 8's column has no
  # crosswalk rows, and 3 and 5 aren't code rules: they pass on.
  expect_equal(issues$class_basis, c(
    "lineage", "lineage", "default", "default", "default", "default", "crosswalk", "default"
  ))
  expect_equal(issues$issue_class[c(1L, 2L, 7L, 8L)], c(
    "harmonization", "harmonization", "harmonization", "unclassified"
  ))
  # An unlisted cell whose code the crosswalk doesn't list passes on, here to its fallback.
  other <- crosswalk
  other$provider_code[[2L]] <- "Sx"
  gpq_classify_issues(
    issues, NULL,
    evidence = list(lineage = lineage, crosswalk = other),
    settings = list(fallback_class = c(code_not_in_list = "source"))
  )
  expect_equal(c(issues$issue_class[[2L]], issues$class_basis[[2L]]), c("source", "fallback"))
  # Without the lineage input, neither step runs (D19.22).
  gpq_classify_issues(issues, NULL, evidence = list(crosswalk = crosswalk))
  expect_equal(issues$class_basis[c(1L, 2L, 7L)], rep("default", 3L))
  # Nor without the crosswalk.
  gpq_classify_issues(issues, NULL, evidence = list(lineage = lineage))
  expect_equal(issues$class_basis[c(1L, 2L, 7L)], rep("default", 3L))
})

test_that("a cell lineage lists with its own value as raw value is unchanged (D19.61, R46)", {
  # Applying D19.61's "the pipeline changed the value": a listed cell whose raw value is its
  # value, as text, passes step 5 on, as an unlisted cell does.
  issues <- fx_class_issues()
  crosswalk <- data.frame(
    provider_code = "Pw", code = "NOP", column_name = "species", water_body = "LK"
  )
  # 7's value is "ZZ", which the crosswalk doesn't list for LK.
  same <- data.frame(
    table_name = "catches", record_pk = "C3", column_name = "species", raw_value = "ZZ",
    raw_source = "catch.sp"
  )
  gpq_classify_issues(issues, NULL, evidence = list(lineage = same, crosswalk = crosswalk))
  expect_equal(c(issues$issue_class[[7L]], issues$class_basis[[7L]]), c("unclassified", "default"))
  # The same cell with another raw value: the pipeline changed it.
  other <- same
  other$raw_value <- "zz"
  gpq_classify_issues(issues, NULL, evidence = list(lineage = other, crosswalk = crosswalk))
  expect_equal(
    c(issues$issue_class[[7L]], issues$class_basis[[7L]]), c("harmonization", "crosswalk")
  )
})

test_that("a blank raw value or provider code matches nothing (R39)", {
  issues <- fx_class_issues()
  # 7's cell is listed with a blank raw value, and the crosswalk lists LK's species with a
  # blank provider code only: the two blanks don't meet, so the code isn't the provider's.
  blank <- data.frame(
    table_name = "catches", record_pk = "C3", column_name = "species", raw_value = NA_character_,
    raw_source = "catch.sp"
  )
  crosswalk <- data.frame(
    provider_code = NA_character_, code = "NOP", column_name = "species", water_body = "LK"
  )
  gpq_classify_issues(issues, NULL, evidence = list(lineage = blank, crosswalk = crosswalk))
  expect_false(identical(issues$class_basis[[7L]], "lineage"))
})

test_that("the evidence's caller errors (D19.28 (7), D19.58 (6))", {
  issues <- fx_class_issues()
  classify <- function(...) gpq_classify_issues(issues, NULL, evidence = list(...))
  group_named <- paste(
    "`computed` must have the columns table_name, column_name, derivation and one more,",
    "named as a group column of the issues (water_body, station_id); it has"
  )
  no_group <- data.frame(table_name = "hauls", column_name = "minutes", derivation = "formula")
  expect_error(classify(computed = no_group), group_named, fixed = TRUE)
  colour <- cbind(no_group, colour = "red")
  expect_error(classify(computed = colour), group_named, fixed = TRUE)
  two <- cbind(no_group, water_body = "*", station_id = "*")
  expect_error(classify(computed = two), group_named, fixed = TRUE)
  guess <- data.frame(
    table_name = "hauls", column_name = "minutes", water_body = "*",
    derivation = c("formula", "guess")
  )
  expect_error(
    classify(computed = guess),
    paste(
      "The derivation at row 2 of `computed` is \"guess\"; a derivation is id, class, formula,",
      "count, fill or reference."
    ),
    fixed = TRUE
  )
  twice <- data.frame(
    table_name = "catches", record_pk = c("C1", "C1"), column_name = "species",
    raw_value = c("Pw", "PW"), raw_source = "catch.sp"
  )
  expect_error(
    classify(lineage = twice),
    "`lineage` lists the cell catches.species of record C1 twice, again at row 2 of `lineage`",
    fixed = TRUE
  )
  expect_error(
    classify(filled = twice[, 1:4]),
    "`filled` must have exactly the columns table_name, record_pk, column_name, written_value",
    fixed = TRUE
  )
  crosswalk <- data.frame(provider_code = "Pw", code = "NOP", column_name = "species")
  expect_error(classify(crosswalk = crosswalk), "`crosswalk` must have the columns", fixed = TRUE)
  # The same branches in their other forms: issues with no group columns, a blank derivation
  # and a derivation in a CSV file, a cell of `filled` twice and a blank cell twice, a CSV file
  # that doesn't read cleanly.
  ungrouped <- fx_class_issues()
  data.table::set(ungrouped, j = c("water_body", "station_id"), value = NULL)
  expect_error(
    gpq_classify_issues(ungrouped, NULL, evidence = list(computed = no_group)),
    "named as a group column of the issues (they have none); it has",
    fixed = TRUE
  )
  unfilled <- guess
  unfilled$derivation <- c("formula", NA)
  expect_error(
    classify(computed = unfilled),
    paste(
      "The derivation at row 2 of `computed` is (blank); a derivation is id, class, formula,",
      "count, fill or reference."
    ),
    fixed = TRUE
  )
  path <- withr::local_tempfile(fileext = ".csv")
  data.table::fwrite(guess, path)
  expect_error(
    classify(computed = path),
    paste0("The derivation at line 3 of ", basename(path), " is \"guess\"; a derivation is id"),
    fixed = TRUE
  )
  filled_twice <- data.frame(
    table_name = "catches", record_pk = c("C1", "C1"), column_name = "species",
    written_value = c("Pw", "PW")
  )
  expect_error(
    classify(filled = filled_twice),
    "`filled` lists the cell catches.species of record C1 twice, again at row 2 of `filled`",
    fixed = TRUE
  )
  filled_twice$record_pk <- NA_character_
  expect_error(
    classify(filled = filled_twice),
    "`filled` lists the cell catches.species of record (blank) twice, again at row 2 of `filled`",
    fixed = TRUE
  )
  ragged <- withr::local_tempfile(fileext = ".csv")
  writeLines(
    c(
      "table_name,record_pk,column_name,written_value", "catches,C1,species,Pw",
      "catches,C2,species,PW,again"
    ),
    ragged
  )
  expect_error(classify(filled = ragged), "`filled` doesn't read cleanly:", fixed = TRUE)
  expect_true(all(is.na(issues$issue_class)))
  # One group column of the issues is all computed needs (after the check above, since this
  # call classes the issues).
  expect_no_error(classify(computed = cbind(no_group, water_body = "*")))
})

test_that("an evidence column stored as integer64 is read as its number (R70, D18.86)", {
  # Found installed without loading it, which skip_if_not_installed() would do: where no
  # earlier test has loaded bit64, classification is what loads it.
  testthat::skip_if(!nzchar(system.file(package = "bit64")), "bit64 isn't installed.")
  # A table kept with saveRDS() and read back where bit64's namespace isn't loaded holds the
  # column as the number's 64 bits in a double. This builds 5000000000 so, loading nothing.
  bits <- writeBin(c(705032704L, 1L), raw(), endian = "little")
  stored <- structure(readBin(bits, "double", endian = "little"), class = "integer64")
  # 7's value is that number as text, and lineage lists its cell with that number as the raw
  # value: unchanged, so it passes on. Read as the double's bits it would differ from the value
  # and be "crosswalk". The crosswalk comes first in the list, with no integer64 column, so the
  # table that has one isn't the first to be looked at.
  issues <- fx_class_issues()
  data.table::set(issues, i = 7L, j = "value", value = "5000000000")
  crosswalk <- data.frame(
    provider_code = "Pw", code = "NOP", column_name = "species", water_body = "LK"
  )
  lineage <- data.frame(
    table_name = "catches", record_pk = "C3", column_name = "species", raw_value = NA,
    raw_source = "catch.sp"
  )
  lineage$raw_value <- stored
  gpq_classify_issues(issues, NULL, evidence = list(crosswalk = crosswalk, lineage = lineage))
  expect_equal(c(issues$issue_class[[7L]], issues$class_basis[[7L]]), c("unclassified", "default"))
  expect_equal(issues$class_basis, rep("default", 8L))
  # The stored bits are bit64's own for that number.
  expect_identical(unclass(stored), unclass(bit64::as.integer64("5000000000")))
})
