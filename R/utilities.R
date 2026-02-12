scale_weights <- function(weights, mode) {
  switch (mode,
    probability = {
      s <- ncol(weights)
      rs <- .rowSums(weights, m = s, n = s)
      pos <- which(rs > 0)
      weights[pos, ] <- weights[pos, ] / rs[pos]
      weights[!pos, ] <- NA
      weights
    },
    minmax = ranger(weights),
    max = weights / max(weights, na.rm = TRUE),
    rank = rank(weights, ties.method = "average")
  )
}

# Functions borrowed from the `tna` package -------------------------------
# https://github.com/sonsoleslp/tna

#' Null coalescing operator
#'
#' Define the null coalescing operator for older R versions
#' @noRd
if (base::getRversion() < "4.4.0") {
  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }
}

#' Normalize `x` to the unit interval from 0 to 1.
#'
#' @param x A `numeric` vector.
#' @param na.rm A `logical` value indicating whether missing values
#'   should be removed.
#' @noRd
ranger <- function(x, na.rm = FALSE) {
  mi <- min(x, na.rm = na.rm)
  ma <- max(x, na.rm = na.rm)
  (x - mi) / (ma - mi)
}

#' Default value operator for a missing argument
#'
#' @param x An \R object
#' @param y An \R object to assign if `x` is missing
#' @noRd
`%m%` <- function(x, y) {
  if (missing(x)) y else x
}

#' Number of unique elements in a vector
#'
#' @param x A `vector`.
#' @noRd
n_unique <- function(x) {
  length(unique(x))
}

# Functions borrowed from the `dynamite` package --------------------------
# https://github.com/ropensci/dynamite

#' Shorthand for `if (test) yes else no`
#'
#' @param test A `logical` value of the condition to evaluate.
#' @param yes An \R object to return when `test` evaluates to `TRUE`.
#' @param no An \R object to return when `test` evaluates to `FALSE`.
#' @noRd
ifelse_ <- function(test, yes, no) {
  if (test) {
    yes
  } else {
    no
  }
}

#' Return `yes` if `test` is `TRUE`, otherwise return `NULL`
#'
#' @param test \[`logical(1)`] Condition to evaluate.
#' @param yes An \R object to return when `test` evaluates to `TRUE`.
#' @noRd
onlyif <- function(test, yes) {
  if (test) {
    yes
  } else {
    NULL
  }
}

#' Generate a Warning Message
#'
#' @param message See [cli::cli_warn()].
#' @param ... See [cli::cli_warn()].
#' @noRd
warning_ <- function(message, ...) {
  cli::cli_warn(message, ..., .envir = parent.frame())
}

#' Stop Function Execution Without Displaying the Call
#'
#' @param message See [cli::cli_abort()].
#' @param ... See [cli::cli_abort()].
#' @param call See [cli::cli_abort()].
#' @noRd
stop_ <- function(message, ..., call = rlang::caller_env()) {
  cli::cli_abort(message, ..., .envir = parent.frame(), call = call)
}

#' Stop function execution unless a condition is true
#'
#' @param message See [cli::cli_abort()].
#' @param ... See [cli::cli_abort()].
#' @param call See [cli::cli_abort()].
#' @noRd
stopifnot_ <- function(cond, message, ..., call = rlang::caller_env()) {
  if (!cond) {
    cli::cli_abort(message, ..., .envir = parent.frame(), call = call)
  }
}

#' Generate an Informative Message
#'
#' @param message See [cli::cli_inform()]
#' @param ... See [cli::cli_inform()]
#' @noRd
message_ <- function(message, ...) {
  cli::cli_inform(message, ..., .envir = parent.frame())
}

#' Create a Comma-separated Character String
#'
#' @param x A `character` vector.
#' @noRd
cs <- function(...) {
  paste0(c(...), collapse = ", ")
}
