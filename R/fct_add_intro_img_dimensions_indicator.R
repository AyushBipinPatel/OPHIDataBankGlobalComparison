#' add_intro_img_dimensions_indicator
#'
#' @description Adds the image explaining the dimensions and respective indicators of the MPI
#'
#' @return Returns a html output for shiny ui to use
#'
#' @noRd

add_intro_img_dimensions_indicator <- function(){
  tags$div(
    
    tags$br(),
    tags$img(src = "www/svg_indicators_dimensions.svg", 
             width = "800px", height = "400px",
             alt = "Image of Dimensions and Indicators"),
    tags$br(),
    style="text-align: center;"
  )
}