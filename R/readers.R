reader_funfact_sequences <- function(cols) {
  force(cols)
  function() {
    private$data <- prepare_sequence_data(private$data_raw, cols)
    private$data_raw <- NULL
  }
}
