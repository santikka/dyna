#' DynaModel R6 Class
#'
#' @export
DynaModel <- R6::R6Class(
  classname = "DynaModel",
  public = list(
    #' @description Create a new DynaModel object.
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
    #' @description Estimate the model.
    estimate = function() {
      private$estimator()
    }
  ),
  private = list(
    data = NULL,
    data_est = NULL,
    data_raw = NULL,
    reader = NULL,
    parser = NULL,
    estimator = NULL,
    estimated = NULL
  )
)
