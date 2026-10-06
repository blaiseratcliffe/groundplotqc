# Writes excel_numbers.xlsx, the workbook fixture of the Excel-number test in
# test-spec_read.R (D12.40): sheet class, header class, over 170.03 and
# -1 stored as numbers, which readxl reads back as the text "170.03" and "-1".
# Run from the repo root: Rscript tests/testthat/fixtures/make_excel_numbers.R
# writexl is used only here, never in R/ or DESCRIPTION.
path <- file.path("tests", "testthat", "fixtures", "excel_numbers.xlsx")
writexl::write_xlsx(list(class = data.frame(class = c(170.03, -1))), path)
cat("Wrote excel_numbers.xlsx.\n")
