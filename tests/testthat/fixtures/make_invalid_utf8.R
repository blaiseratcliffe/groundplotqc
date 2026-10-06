# Writes invalid_utf8.xlsx, the workbook fixture of the invalid-byte tests in
# test-spec_read.R (D12.28): sheet codes, header id,
# comments and "na<0x97>me", over rows 1, fine, x and 2, "ok<0x97>", y. It then reads the
# file back and stops, deleting it, unless both invalid bytes survive the round trip.
# Run from the repo root: Rscript tests/testthat/fixtures/make_invalid_utf8.R
# writexl is used only here, never in R/ or DESCRIPTION.
bytes_to_text <- function(bytes) {
  x <- rawToChar(as.raw(bytes))
  Encoding(x) <- "UTF-8"
  x
}
bad_cell <- bytes_to_text(c(0x6F, 0x6B, 0x97))
bad_name <- bytes_to_text(c(0x6E, 0x61, 0x97, 0x6D, 0x65))
codes <- data.frame(id = c("1", "2"), comments = c("fine", bad_cell), third = c("x", "y"))
names(codes)[[3L]] <- bad_name
path <- file.path("tests", "testthat", "fixtures", "invalid_utf8.xlsx")
writexl::write_xlsx(list(codes = codes), path)
back <- readxl::read_excel(
  path,
  sheet = "codes", col_names = FALSE, col_types = "text", trim_ws = FALSE,
  .name_repair = "minimal"
)
survived <- identical(charToRaw(back[[3L]][[1L]]), charToRaw(bad_name)) &&
  identical(charToRaw(back[[2L]][[3L]]), charToRaw(bad_cell))
if (!survived) {
  file.remove(path)
  stop(
    "The invalid byte didn't survive writexl and readxl: invalid_utf8.xlsx isn't kept. ",
    "Per D12.28, the workbook branch is then tested with a mocked read_xlsx_raw().",
    call. = FALSE
  )
}
cat("Wrote invalid_utf8.xlsx; both invalid bytes survive a read with readxl.\n")
