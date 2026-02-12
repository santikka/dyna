prepare_sequence_data <- function(x, cols) {
  alphabet <- attr(x, "alphabet")
  if (is.null(alphabet)) {
    vals <- as.character(sort(unique(unlist(x[, cols]))))
    alphabet <- vals[!is.na(vals) & nchar(vals) > 0]
  }
  x <- x[, cols] |>
    lapply(
      function(y) {
        y <- factor(y, levels = alphabet)
        as.integer(replace(y, which(!y %in% alphabet), NA))
      }
    ) |>
    as.data.frame() |>
    as.matrix()
  list(
    sequences = x,
    alphabet = alphabet
  )
}

#' Get specific columns from data
#'
#' @param expr An `expression` for the columns to select.
#' @param data A `data.frame` to select the columns from.
#' @noRd
get_cols <- function(expr, data) {
  if (rlang::quo_is_null(expr) || rlang::quo_is_missing(expr)) {
    return(NULL)
  }
  if (rlang::quo_is_symbolic(expr) && !rlang::quo_is_call(expr, "!!")) {
    pos <- tidyselect::eval_select(expr = expr, data = data)
    names(pos)
  } else {
    cols <- rlang::eval_tidy(expr = expr)
    if (is.character(cols)) {
      cols_mis <- setdiff(cols, names(data))
      stopifnot_(
        length(cols_mis) == 0L,
        c(
          "Can't select columns that don't exist.",
          `x` = "Column {.var {cols_mis[1L]}} doesn't exist."
        )
      )
      return(cols)
    }
    if (is.numeric(cols)) {
      nm <- names(data)
      k <- length(nm)
      cols_mis <- setdiff(cols, seq_len(k))
      stopifnot_(
        length(cols_mis) == 0L,
        c(
          "Can't select columns that don't exist.",
          `x` = "Attempted to select column {cols_mis[1]} from {k} columns."
        )
      )
      return(nm[cols])
    }
    stop_(
      "Columns must be selected using a tidy selection,
       a {.cls character} vector, or an {.cls integer} vector."
    )
  }
}
