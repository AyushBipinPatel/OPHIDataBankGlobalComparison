#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  mod_aggregate_measures_server("aggregate_measures_ui_1")
  mod_headcount_indicators_server("headcount_indicators_ui_1")
}
