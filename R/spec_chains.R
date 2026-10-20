# Chains of foreign keys in D18.1's " > " notation (D18.1, D18.2, D18.27): from a table, each
# foreign key followed in turn and the attribute read where the keys end, such as
# "magp_dataset_id > magp_contributor_id" from magp_sites. One parser and one resolver serve
# id_bands' key (site_id_out_of_range) and, from Layer 4, key_pairs' sides; reach_column()
# follows its shortest path with the same resolver.

#' A chain parsed against the dictionary's keys and attributes
#'
#' `chain` is one text of parts joined by " > ": each part but the last a foreign key of the
#' table reached so far (a `keys` row of type FK naming its parent's PK), the last an
#' attribute of the table the keys reach; a chain of one part is an attribute of `table`
#' itself. Returns `steps` (the FK rows followed, in order, as fk_path() gives a path),
#' `table` (the table reached) and `attribute` (read there), or NULL where the chain doesn't
#' parse: a blank or padded part, a part that isn't such a key, or a last part the table
#' reached doesn't have.
#' @noRd
parse_chain <- function(chain, table, keys, attributes) {
  if (!is.character(chain) || length(chain) != 1L || is.na(chain)) {
    return(NULL)
  }
  parts <- strsplit(chain, " > ", fixed = TRUE)[[1L]]
  # strsplit() drops a trailing empty part, so the parts must give the chain back exactly.
  shaped <- length(parts) > 0L && identical(paste(parts, collapse = " > "), chain) &&
    all(nzchar(parts) & parts == trimws(parts))
  if (!shaped) {
    return(NULL)
  }
  is_edge <- keys$key_type == "FK" & !is.na(keys$reference_table) &
    !is.na(keys$reference_attribute)
  edges <- keys[is_edge]
  steps <- integer()
  current <- table
  for (part in parts[-length(parts)]) {
    edge <- which(edges$table_name == current & edges$attribute_name == part)
    if (length(edge) != 1L) {
      return(NULL)
    }
    steps <- c(steps, edge)
    current <- edges$reference_table[[edge]]
  }
  last <- parts[[length(parts)]]
  if (!any(attributes$table_name == current & attributes$attribute_name == last)) {
    return(NULL)
  }
  list(steps = edges[steps], table = current, attribute = last)
}

#' Each row of a table followed along foreign keys: its row in the table the keys reach
#'
#' `steps` are FK rows in order (parse_chain()'s `steps`, or fk_path()'s path). Each hop is
#' one match() of the column as given against its parent's PK (key_values()): a column stored
#' as plain numbers joins as stored, and against text only the numeric side is written as
#' text, its sentinels with it (D16.42, m9); NA, every sentinel of the child's and the
#' parent's types, and a blank key as space_padding() reads it (Unicode spaces included) match
#' nothing, whatever the parent's key holds (D16.26; D16.42, m8; D18.31). They are masked after
#' a plain match(), read with %in% in the key's own type rather than in a copy of the column
#' (16.2), as match()'s `incomparables` isn't always honoured when it holds more than one value
#' (D18.73). A row whose chain breaks (an NA, blank, sentinel or orphan key on the way, or a
#' table or column on it not given) gets NA; with no steps every row is its own.
#' @noRd
chain_rows <- function(input, table, steps) {
  rows <- seq_len(nrow(input$data[[table]]))
  current <- table
  # The blank values of a text key, read only where they may be blank (16.2). A blank on the
  # parent's side can only meet the same blank on the child's, so the child's suffice.
  blanks <- function(x) {
    if (!is.character(x)) {
      return(NULL)
    }
    maybe <- which(grepl("^[ \t\r\n]*$|[^\001-\177]", x, useBytes = TRUE))
    unique(x[maybe][space_padding(x[maybe])$blank])
  }
  for (k in seq_len(nrow(steps))) {
    parent <- steps$reference_table[[k]]
    child <- key_values(input, current, steps$attribute_name[[k]])
    parent_keys <- key_values(input, parent, steps$reference_attribute[[k]])
    # The first step matches the child's column itself, a later one the rows reached (16.2).
    from <- if (k == 1L) child$value else child$value[rows]
    to <- parent_keys$value
    from_open <- child$open
    to_open <- parent_keys$open
    if (is.numeric(from) && is.character(to)) {
      from <- value_text(from)
      from_open <- value_text(from_open)
    } else if (is.character(from) && is.numeric(to)) {
      to <- value_text(to)
      to_open <- value_text(to_open)
    }
    rows <- match(from, to)
    # The keys that match nothing, NA where the child's key is one: a parent's key equal to one
    # is one too. Held in the child's type, so %in% writes no number key as text (16.2): a
    # parent's column not given has no codes, which c() would otherwise make text.
    masked <- c(NA, from_open, to_open, blanks(from))
    if (is.numeric(from)) {
      masked <- suppressWarnings(as.numeric(masked))
    }
    rows[from %in% masked] <- NA_integer_
    current <- parent
  }
  rows
}

#' A chain's value for each row of a table: the attribute where its keys end, as given, NA
#' where the chain breaks or the column wasn't given
#'
#' `chain` is parse_chain()'s result.
#' @noRd
chain_values <- function(input, table, chain) {
  reached_values(
    input, list(home = chain$table, rows = chain_rows(input, table, chain$steps)), chain$attribute
  )
}
