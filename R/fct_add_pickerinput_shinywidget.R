#' add_pickerinput_shinywidget 
#'
#' @param inputID parameter to set inputID to the pickerInput function
#' @param label parameter to set label to the pickerInput function
#' @param choices parameter to set possible options to the pickerInput function
#' @param selected parameter to set default selection to the pickerInput function
#'
#' @description A wrapper on pickerInput deom shinywidgets
#'
#' @return The returns html in to be generated in the UI.
#'
#' @noRd


add_pickerinput_shinywidget <- function(inputID,label,choices,selected){
  shinyWidgets::pickerInput(
    inputId = inputID,
    label = label,
    choices = choices,
    selected = selected,
    options = list(
      `live-search` = TRUE)
  )
}