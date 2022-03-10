#' add_intro_method_note 
#'
#'
#' @description Adds text for methodological notes, relevant links and suggested citation
#'
#' @return HTML version of text for shiny to show on the UI
#'
#' @noRd

add_intro_method_note <- function(){
  
  tags$div(
    tags$p(
      "A brief methodological note is published following each round of global MPI update. For the global MPI 2021 update, please refer to Alkire et al. (2021) which can be found on the",
      HTML('<a href="https://ophi.org.uk/publications/mpi-methodological-notes/">Methodological Notes</a>'), "page. The note explains the methodological adjustments that were made while revising and standardizing indicators for over 100 countries."
    ),
    tags$br(),
    tags$p(
      "The suggested citation for this interactive databank is:",tags$br(),
      tags$strong("Alkire, S., Kanagaratnam, U. and Suppa, N. (2021). ‘The Global Multidimensional Poverty Index (MPI) 2021. OPHI MPI Methodological Note 51,Oxford Poverty and Human Development Initiative, University of Oxford.")
    ),
    tags$br(),
    tags$p(
      "On this page, data can be browsed at the national level and on the Urban/Rural level where disaggregations are available."
    )
  )
  
}