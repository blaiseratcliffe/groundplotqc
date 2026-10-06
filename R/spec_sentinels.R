# The sentinel table (plan 3.5; D1.8, D7.4, D8.8, D9.18, D12.19). No built-in values:
# the MAGPlot layer supplies -1/-9 and X/Z. Rows attach to dictionary types by family.

#' Declare missing-value sentinels
#'
#' @description
#' Builds the table of sentinel values: the codes a column holds when a value is missing
#' or doesn't apply. The engine has none built in.
#'
#' @details
#' Each row belongs to a family, not to one dictionary type: `numeric` rows
#' cover every type the type map gives R class integer or double; `date` rows every type
#' whose type-map row has a `date_format`; `character` rows every other type of R class
#' character. A type no row covers has no sentinels, and the rules that need them record
#' `not_run` for its columns. A primary key never holds a sentinel; a foreign key may hold
#' the not-applicable one.
#'
#' @param numeric,character,date `NULL`, or a named vector with names `missing` and
#'   `not_applicable`, for example `c(missing = -1, not_applicable = -9)`.
#' @return A data.table with columns `data_type` (the family), `role`, `value` (as text),
#'   `allowed_in_pk` and `allowed_in_fk`.
#' @examples
#' gpq_sentinels(
#'   numeric = c(missing = -99, not_applicable = -88),
#'   character = c(missing = "?", not_applicable = "~"),
#'   date = c(missing = "?", not_applicable = "~")
#' )
#' @export
gpq_sentinels <- function(numeric = NULL, character = NULL, date = NULL) {
  families <- list(numeric = numeric, character = character, date = date)
  rows <- lapply(names(families), function(family) {
    values <- families[[family]]
    if (is.null(values)) {
      return(NULL)
    }
    roles <- names(values)
    bad <- is.null(roles) || !all(roles %in% c("missing", "not_applicable")) ||
      anyDuplicated(roles) > 0L || anyNA(values)
    if (bad) {
      stop(sprintf(
        "`%s` must be a vector named missing and not_applicable.", family
      ), call. = FALSE)
    }
    data.table(
      data_type = family, role = roles, value = as_text(unname(values)),
      allowed_in_pk = FALSE, allowed_in_fk = roles == "not_applicable"
    )
  })
  validate_sentinels(rbindlist(rows))
}

#' A sentinel table checked, families and roles as approved (D12.19)
#' @noRd
validate_sentinels <- function(sentinels) {
  columns <- c("data_type", "role", "value", "allowed_in_pk", "allowed_in_fk")
  if (!is.data.frame(sentinels)) {
    stop("A sentinel table must be a data.frame, as gpq_sentinels() makes.", call. = FALSE)
  }
  if (nrow(sentinels) == 0L && ncol(sentinels) == 0L) {
    return(data.table(
      data_type = vector("character", 0L), role = vector("character", 0L),
      value = vector("character", 0L), allowed_in_pk = logical(), allowed_in_fk = logical()
    ))
  }
  if (!setequal(names(sentinels), columns)) {
    stop("A sentinel table has the columns ", paste(columns, collapse = ", "), ".", call. = FALSE)
  }
  # A repeated name would have its first copy used and the rest dropped silently (D12.54).
  repeated <- unique(names(sentinels)[duplicated(names(sentinels))])
  if (length(repeated) > 0L) {
    stop(
      "A sentinel table's column names must not repeat: ", paste(repeated, collapse = ", "), ".",
      call. = FALSE
    )
  }
  # A key flag is a logical or the text TRUE or FALSE, as a file holds it; as.logical() would
  # also take 0, 1, "T" and "true", which the message below doesn't offer.
  as_flag <- function(x) c(TRUE, FALSE)[match(as_text(x), c("TRUE", "FALSE"))]
  out <- data.table(
    data_type = as_text(sentinels$data_type), role = as_text(sentinels$role),
    value = as_text(sentinels$value),
    allowed_in_pk = as_flag(sentinels$allowed_in_pk),
    allowed_in_fk = as_flag(sentinels$allowed_in_fk)
  )
  if (!all(out$data_type %in% c("numeric", "character", "date"))) {
    stop("A sentinel row's data_type is numeric, character or date (D12.19).", call. = FALSE)
  }
  if (anyNA(out$allowed_in_pk) || anyNA(out$allowed_in_fk)) {
    stop("A sentinel row's allowed_in_pk and allowed_in_fk are TRUE or FALSE.", call. = FALSE)
  }
  if (!all(out$role %in% c("missing", "not_applicable")) || anyNA(out)) {
    stop("A sentinel row has a role missing or not_applicable and no blank cell.", call. = FALSE)
  }
  if (anyDuplicated(out, by = c("data_type", "role")) > 0L) {
    stop("A sentinel table has one row for each data_type and role (D12.54).", call. = FALSE)
  }
  # A file's bad bytes are already <xx>; one given in R is a caller's error (D12.54).
  if (!all(validUTF8(out$value))) {
    stop("A sentinel value must be valid UTF-8 text.", call. = FALSE)
  }
  out
}
