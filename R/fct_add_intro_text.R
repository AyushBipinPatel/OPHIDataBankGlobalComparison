#' add_intro_text
#'
#' @description This function is created to generate the intro text on the about page
#'
#' @return It returns html version of the desired text by using functions from the package {shiny}.
#'
#' @noRd


add_intro_text <- function(){
  
  return(
    tags$div(
      h1(tags$b(tags$span(style = "color:#7c1419","Global Comparision"))),
      tags$div(
        tags$p("The global Multidimensional Poverty Index (MPI) was created using the multidimensional measurement method of Alkire and Foster (AF). The global MPI is an index of acute multidimensional poverty that covers over 100 countries. It is computed using data from the most recent Demographic and Health Surveys (DHS), Multiple Indicator Cluster Surveys (MICS), Pan Arab Project for Family Health (PAPFAM) and national surveys. The MPI has three dimensions and 10 indicators. Each dimension is equally weighted, and each indicator within a dimension is also equally weighted. Any person who fails to meet the deprivation cutoff is identified as deprived in that indicator. So the core information the MPI uses is the profile of deprivations each person experiences.")
      )
    )
  )
}