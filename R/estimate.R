#' Estimate a Network Model
#'
#' @export
#' @param data TODO
#' @param cols TODO
#' @param type TODO
#' @return TODO
estimate_model <- function(data, cols = tidyselect::everything(),
                           scaling = character(0L)) {
  params <- list()
  params$scaling <- scaling
  cols <- get_cols(rlang::enquo(cols), data)
  model <- DynaNetwork$new(data)
  model$set_reader(reader_funfact_sequences, cols)
  model$set_parser(parser_funfact_transitions)
  model$set_estimator(estimator_funfact_weight_matrix, params)
  model$read()
  model$parse()
  model$estimate()
  model
}
