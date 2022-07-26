#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    fluidPage(
      shiny::navbarPage(theme = bslib::bs_theme(bootswatch = "minty", version  = 4),
                        title = "OPHI Data Bank - Global Comparisions",
                        shiny::tabPanel(title = "About",
                                        add_intro_text(),
                                        add_intro_img_dimensions_indicator(),
                                        add_intro_method_note()
                        ),
                        shiny::tabPanel(
                          title = "Aggregate Measures",
                          mod_aggregate_measures_ui("aggregate_measures_ui_1")
                          
                        ),
                        shiny::tabPanel(
                          title = "Headcount of Poverty",
                          mod_headcount_poverty_ui("headcount_poverty_ui_1")
                        ),
                        shiny::tabPanel(
                          title = "Deprivations in Indicators",
                          mod_headcount_indicators_ui("headcount_indicators_ui_1")
                        ),
                        shiny::tabPanel(
                          title = "Contribution of Indicators",
                          mod_contribution_indicators_ui("contribution_indicators_ui_1")
                          )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'OPHIGlobalComparisions'
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
    tags$link(
      rel="stylesheet", 
      type="text/css", 
      href="www/custom.css"
    )
  )
}

