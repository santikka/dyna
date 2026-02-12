#' DynaNetwork R6 Class
#'
#' @export
DynaNetwork <- R6::R6Class(
  classname = "DynaNetwork",
  public = list(
    #' @description Create a new DynaNetwork object.
    #' @param data The input `data` for the model.
    initialize = function(data) {
      private$data <- data
    },
    #' @description Set the `data` field.
    #' @param data A `data.frame` object.
    set_data = function(data) {
      private$data <- data
    },
    #' @description Set the `estimator` field.
    #' @param fun A `function`.
    #' @param cols TODO
    set_parser = function(fun, cols) {
      f <- fun(cols)
      environment(f) <- environment()
      private$parser <- f
    },
    #' @description Set the `estimator` field.
    #' @param fun A `function`.
    #' @param params Addtional estimator parameters..
    set_estimator = function(fun, params) {
      f <- fun(params)
      environment(f) <- environment()
      private$estimator <- f
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
    estimation_data = NULL,
    estimator = NULL,
    parser = NULL
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
