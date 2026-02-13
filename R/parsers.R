parser_funfact_sequences_transitions <- function() {
  function() {
    sequences <- private$data$sequences
    alphabet <- private$data$alphabet
    k <- length(alphabet)
    n <- nrow(sequences)
    p <- ncol(sequences)
    transitions <- array(0L, dim = c(n, k, k))
    idx_mat <- cbind(seq_len(n), matrix(0L, nrow = n, ncol = 2L))
    from <- sequences[, 1L]
    missing_from <- is.na(from)
    for (i in seq_len(p - 1L)) {
      to <- sequences[, i + 1L]
      missing_to <- is.na(to)
      missing_either <- missing_from | missing_to
      idx_mat[, 2L] <- from
      idx_mat[, 3L] <- to
      idx_new <- idx_mat[!missing_either, , drop = FALSE]
      transitions[idx_new] <- transitions[idx_new] + 1L
      from <- to
      missing_from <- missing_to
    }
    private$data_est <- transitions
  }
}

parser_funfact_sequences_cooccurrence <- function() {
  function() {
    sequences <- private$data$sequences
    alphabet <- private$data$alphabet
    k <- length(alphabet)
    n <- nrow(sequences)
    p <- ncol(sequences)
    co <- array(0L, dim = c(n, k, k))
    idx_mat <- cbind(
      rep(seq_len(n), 2L),
      matrix(0L, nrow = 2 * n, ncol = 2L)
    )
    for (i in seq_len(p - 1L)) {
      for (j in seq(i + 1L, p)) {
        from <- sequences[, i]
        to <- sequences[, j]
        missing_either <- rep(is.na(from) | is.na(to), 2L)
        idx_mat[, 2L] <- c(from, to)
        idx_mat[, 3L] <- c(to, from)
        idx_new <- idx_mat[!missing_either, , drop = FALSE]
        co[idx_new] <- co[idx_new] + 1L
      }
    }
    private$data_est <- co
  }
}

# TODO onehot variants
