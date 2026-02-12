parser_funfact_transitions <- function(cols) {
  force(cols)
  function() {
    seqdata <- prepare_sequence_data(private$data, cols)
    sequences <- seqdata$sequences
    k <- length(seqdata$alphabet)
    n <- nrow(sequences)
    p <- ncol(sequences)
    transitions <- array(0L, dim = c(n, k, k))
    idx_mat <- cbind(seq_len(n), matrix(0L, nrow = n, ncol = 2L))
    from <- sequences[, 1L]
    missing_from <- is.na(from)
    for (i in seq_len(p - 1L)) {
      to <- sequences[, i + 1L]
      missing_to <- is.na(to)
      missing_any <- missing_from | missing_to
      idx_mat[, 2L] <- from
      idx_mat[, 3L] <- to
      idx_new <- idx_mat[!missing_any, , drop = FALSE]
      transitions[idx_new] <- transitions[idx_new] + 1L
      from <- to
      missing_from <- missing_to
    }
    attr(transitions, "alphabet") <- seqdata$alphabet
    private$estimation_data <- transitions
  }
}
