# Writes blank_cells.xlsx, the workbook fixture of the blank-cell tests in
# test-spec_read.R (D12.27): a header "value" over "", "   ", "NA" and " NT_PSP".
# writexl writes no cell for the "" (row 2 is absent from the sheet), so that case is a
# missing cell, read as NA like it; the other three cases carry the test (D12.27).
# Run from the repo root: Rscript tests/testthat/fixtures/make_blank_cells.R
# writexl is used only here, never in R/ or DESCRIPTION.
cells <- data.frame(value = c("", "   ", "NA", " NT_PSP"))
path <- file.path("tests", "testthat", "fixtures", "blank_cells.xlsx")
writexl::write_xlsx(cells, path)
