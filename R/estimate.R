#' Estimate a Network Model
#'
#' @export
#' @param data TODO
#' @param format TODO
#' @param type TODO
#' @param cols TODO
#' @param scaling TODO
#' @return TODO
estimate_model <- function(data, format = "sequences", type = "tna",
                           cols = tidyselect::everything(),
                           scaling = character(0L)) {
  params <- list()
  params$scaling <- scaling
  params <- get_params_spec(type, params)
  cols <- get_cols(rlang::enquo(cols), data)
  model <- DynaModel$new(data)
  model$set_reader(get_reader_spec(format), cols)
  model$set_parser(get_parser_spec(format, type))
  model$set_estimator(get_estimator_spec(type), params)
  model$read()
  model$parse()
  model$estimate()
  model
}

get_params_spec <- function(type, params) {
  if (type == "tna") {
    params$scaling <- c("probability", params$scaling)
  }
  params
}

get_reader_spec <- function(format) {
  switch (format,
    sequences = reader_funfact_sequences,
    onehot = reader_funfact_onehot
  )
}

# TODO onehot aggregation option?
get_parser_spec <- function(format, type) {
  if (format == "sequences") {
    switch (type,
      tna = parser_funfact_sequences_transitions,
      ftna = parser_funfact_sequences_transitions,
      cna = parser_funfact_sequences_cooccurrence
    )
  } else if (format == "onehot") {
    switch (type,
      tna = parser_funfact_onehot_transitions,
      ftna = parser_funfact_onehot_transitions,
      cna = parser_funfact_onet_cooccurrence
    )
  }
}

get_estimator_spec <- function(type) {
  switch (type,
    tna = estimator_funfact_weight_matrix,
    ftna = estimator_funfact_weight_matrix,
    cna = estimator_funfact_weight_matrix
  )
}
