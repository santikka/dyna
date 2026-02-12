#' DynaNetwork R6 Class
#'
#' @export
DynaNetwork <- R6::R6Class(
  classname = "DynaNetwork",
  public = list(
    #' @description Create a new DynaNetwork object.
    #' @param data The input `data` for the model.
    initialize = function(data) {
      private$data_raw <- data
    },
    #' @description Set the `parser` field.
    #' @param fun A `function`.
    #' @param cols TODO
    set_reader = function(fun, cols) {
      f <- fun(cols)
      environment(f) <- environment()
      private$reader <- f
    },
    #' @description Set the `parser` field.
    #' @param fun A `function`.
    #' @param cols TODO
    set_parser = function(fun) {
      f <- fun()
      environment(f) <- environment()
      private$parser <- f
    },
    #' @description Set the `estimator` field.
    #' @param fun A `function`.
    #' @param params A `list` of additional estimator parameters.
    set_estimator = function(fun, params) {
      f <- fun(params)
      environment(f) <- environment()
      private$estimator <- f
    },
    #' @description Read raw data for processing.
    read = function() {
      private$reader()
    },
    #' @description Parse data for estimation.
    parse = function() {
      private$parser()
    },
    #' @description Estimate the network model.
    estimate = function() {
      private$estimator()
    },
    #' @description Get estimated weights
    get_weights = function() {
      private$weights
    }
  ),
  private = list(
    nodes = NULL,
    edges = NULL,
    weights = NULL,
    data = NULL,
    data_est = NULL,
    data_raw = NULL,
    reader = NULL,
    parser = NULL,
    estimator = NULL
  ),
  active = list(
    #' @field n_nodes Number of nodes in the network.
    n_nodes = function() {
      nrow(private$nodes) %||% 0L
    },
    #' @field n_edges Number of edges in the network.
    n_edges = function() {
      nrow(private$edges) %||% 0L
    }
  )
)
