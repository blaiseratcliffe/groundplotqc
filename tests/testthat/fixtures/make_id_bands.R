# Writes id_bands.xlsx, the workbook fixture of the site-ID test in test-preflight.R
# (D12.28): sheet ranges (contributor, v2_start, v2_end: AA 1 100, BB 50 150, CC 300 250)
# and sheet contributor (name: AA, BB, CC), every cell text.
# Run from the repo root: Rscript tests/testthat/fixtures/make_id_bands.R
# writexl is used only here, never in R/ or DESCRIPTION.
ranges <- data.frame(
  contributor = c("AA", "BB", "CC"), v2_start = c("1", "50", "300"),
  v2_end = c("100", "150", "250")
)
contributor <- data.frame(name = c("AA", "BB", "CC"))
path <- file.path("tests", "testthat", "fixtures", "id_bands.xlsx")
writexl::write_xlsx(list(ranges = ranges, contributor = contributor), path)
cat("Wrote id_bands.xlsx.\n")
