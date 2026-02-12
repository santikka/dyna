estimator_funfact_transitions <- function(params) {
  force(params)
  function() {
    alphabet <- attr(private$estimation_data, "alphabet")
    weights <- apply(private$estimation_data, 2:3, sum)
    for (i in seq_along(params$scaling)) {
      weights[] <- scale_weights(weights, mode = params$scaling[i])
    }
    rownames(weights) <- alphabet
    colnames(weights) <- alphabet
    private$weights <- weights
  }
}
