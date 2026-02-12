#' Estimate a Network Model
#'
#' @export
#' @param data TODO
#' @param cols TODO
#' @param type TODO
#' @return TODO
estimate_model <- function(data, cols = tidyselect::everything()) {
  params <- list()
  cols <- get_cols(rlang::enquo(cols), data)
  model <- DynaNetwork$new(data)
  model$set_parser(parser_funfact_tna, cols)
  model$set_estimator(estimator_funfact_tna, params)
  model$parse()
  model$estimate()
  model
}
