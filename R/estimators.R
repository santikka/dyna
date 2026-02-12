estimator_funfact_weight_matrix <- function(params) {
  force(params)
  function(keep_data = FALSE) {
    alphabet <- attr(private$data_est, "alphabet")
    weights <- apply(private$data_est, 2:3, sum)
    for (i in seq_along(params$scaling)) {
      weights[] <- scale_weights(weights, mode = params$scaling[i])
    }
    rownames(weights) <- alphabet
    colnames(weights) <- alphabet
    if (!keep_data) {
      private$data_est <- NULL
    }
    private$weights <- weights
  }
}
