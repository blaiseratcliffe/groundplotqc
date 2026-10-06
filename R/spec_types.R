# The column map and the type map (plan 3.4, 4.1; D2.15, D12.13, D12.19, D12.22).

#' Name the dictionary's columns
#'
#' @description
#' Tells [gpq_read_spec()] which column of the data dictionary holds each piece of
#' information. Each default is the column name shown in the usage above.
#'
#' @param table,attribute,key_type,reference,lookup,description One column name each:
#'   the table, the attribute, the key type (`"PK"` or `"FK"`, in any case), the table a
#'   foreign key refers to, the code list (a sheet or translation table by name, or `"Y"`,
#'   in any case, for the sheet named after the attribute), and the description.
#' @param type One or more names the type column may have. Exactly one of them must be in
#'   the dictionary; otherwise pre-flight stops with `dd_type_column_ambiguous`.
#' @param lineage_flag `NULL`, or `c(column = , value = )`: the dictionary column, and the
#'   value in it, that mark the attributes needing a row in the lineage spec.
#' @return A list of class `gpq_column_map`, with elements `table`, `attribute`, `type`,
#'   `key_type`, `reference`, `lookup`, `description` and `lineage_flag`.
#' @examples
#' # The fish survey example names its dictionary columns its own way.
#' fish_map <- gpq_column_map(
#'   table = "table", attribute = "field", type = "kind", key_type = "key",
#'   reference = "parent", lookup = "codes", description = "notes"
#' )
#' fish_map$attribute
#' @export
gpq_column_map <- function(table = "table_name", attribute = "attribute_name",
                           type = c("data_type", "datatype"), key_type = "key_type",
                           reference = "reference_table", lookup = "lookup_table",
                           description = "description", lineage_flag = NULL) {
  single <- list(
    table = table, attribute = attribute, key_type = key_type,
    reference = reference, lookup = lookup, description = description
  )
  # A name given in R is valid UTF-8 text, never blank: empty after trimming, as D12.27's
  # blank cells are, since such a name can never match (D12.54).
  names_ok <- function(x) is.character(x) && !anyNA(x) && !any(is_blank(x)) && all(validUTF8(x))
  for (role in names(single)) {
    value <- single[[role]]
    if (length(value) != 1L || !names_ok(value)) {
      stop(sprintf("`%s` must be one column name.", role), call. = FALSE)
    }
  }
  if (length(type) < 1L || !names_ok(type)) {
    stop("`type` must be one or more column names.", call. = FALSE)
  }
  flag_ok <- is.null(lineage_flag) || names_ok(lineage_flag) &&
    length(lineage_flag) == 2L && setequal(names(lineage_flag), c("column", "value"))
  if (!flag_ok) {
    stop("`lineage_flag` must be NULL or c(column = , value = ).", call. = FALSE)
  }
  map <- c(
    single[c("table", "attribute")], list(type = type),
    single[c("key_type", "reference", "lookup", "description")],
    list(lineage_flag = lineage_flag[c("column", "value")])
  )
  structure(map, class = "gpq_column_map")
}

#' Map dictionary types to R classes
#'
#' @description
#' The type map says which R class each type of the data dictionary expects, and which
#' types hold dates.
#'
#' @param map `NULL` for the four built-in rows, or a data.frame or the path of a CSV file,
#'   ending in `.csv`, with columns `data_type`, `r_class` and `date_format`, which replaces
#'   them.
#' @return A data.table with columns `data_type` (the dictionary's type name), `r_class`
#'   (`"character"`, `"integer"` or `"double"`) and `date_format` (a [strptime()] format
#'   that non-sentinel values must parse with, or `NA`). Built in: `character`, `integer`,
#'   `numeric` (double) and `date` (character, `"%Y-%m-%d"`). A map read from a file
#'   carries what was found reading it, an invalid byte or a malformed line, as its
#'   attribute `gpq_read_findings`, which [gpq_read_spec()] adds to its `read_findings`.
#'   Nothing is signalled here: [gpq_preflight()] reports them, and
#'   `attr(map, "gpq_read_findings")` shows them.
#' @examples
#' gpq_type_map()
#' path <- system.file("extdata", "examples", "fish_types.csv", package = "groundplotqc")
#' fish <- gpq_type_map(path)
#' attr(fish, "gpq_read_findings")
#' @export
gpq_type_map <- function(map = NULL) {
  findings <- NULL
  if (is.null(map)) {
    map <- data.table(
      data_type = c("character", "integer", "numeric", "date"),
      r_class = c("character", "integer", "double", "character"),
      date_format = c(NA, NA, NA, "%Y-%m-%d")
    )
  } else if (is.character(map) && length(map) == 1L) {
    if (is.na(map) || !file.exists(map) || dir.exists(map)) {
      stop("`map` names a file that doesn't exist.", call. = FALSE)
    }
    if (!grepl("[.]csv$", map, ignore.case = TRUE)) {
      stop("`map` must be the path of a file ending in .csv.", call. = FALSE)
    }
    # The file's findings ride on the map until gpq_read_spec() reads them (D12.55).
    read <- read_input_table(map, "type_map")
    map <- read$data
    findings <- read$findings
  } else if (!is.data.frame(map)) {
    stop("`map` must be NULL, a data.frame or the path of a CSV file.", call. = FALSE)
  }
  out <- validate_type_map(map)
  if (!is.null(findings)) {
    setattr(out, "gpq_read_findings", findings)
  }
  out
}

#' A type map checked, its columns as text
#' @noRd
validate_type_map <- function(map) {
  columns <- c("data_type", "r_class", "date_format")
  if (!setequal(names(map), columns)) {
    stop("A type map has the columns data_type, r_class and date_format.", call. = FALSE)
  }
  # A repeated name would have its first copy used and the rest dropped silently (D12.54).
  repeated <- unique(names(map)[duplicated(names(map))])
  if (length(repeated) > 0L) {
    stop(
      "A type map's column names must not repeat: ", paste(repeated, collapse = ", "), ".",
      call. = FALSE
    )
  }
  map <- as.data.table(lapply(as.list(map)[columns], as_text))
  # A file's bad bytes are already <xx>; one given in R is a caller's error (D12.54).
  if (!all(validUTF8(unlist(map, use.names = FALSE)))) {
    stop("A type map's text must be valid UTF-8.", call. = FALSE)
  }
  if (anyNA(map$data_type) || anyDuplicated(map$data_type) > 0L) {
    stop("A type map's data_type must be filled and unique.", call. = FALSE)
  }
  if (!all(map$r_class %in% c("character", "integer", "double"))) {
    stop("A type map's r_class is character, integer or double.", call. = FALSE)
  }
  if (any(!is.na(map$date_format) & map$r_class != "character")) {
    stop("A type map's date_format is only for types of r_class character.", call. = FALSE)
  }
  map
}

#' The path of a file in inst/extdata/examples/
#' @noRd
example_file <- function(name) {
  system.file("extdata", "examples", name, package = "groundplotqc", mustWork = TRUE)
}
