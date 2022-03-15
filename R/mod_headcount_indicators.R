#' headcount_indicators UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_headcount_indicators_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::fluidPage(
      
      shiny::tags$div(
        h1(shiny::tags$b(shiny::tags$span(style = "color:#7c1419","Censored and Uncensored Headcount Ratios Of Indicators"))),
        tags$div(
          shiny::tags$p("The AF methodology has a property that makes the global MPI even more useful: dimensional breakdown. This property makes it possible to compute the percentage of the population who are multidimensionally poor and simultaneously deprived in each indicator. This is known as the censored headcount ratio of an indicator. The following visualisation shows the censored headcount ratio of each indicator at the national level. Poverty information, however, becomes even more valuable when it is disaggregated by urban and rural areas. It also allows the breakdown of indicators by country, and urban and rural areas. This analysis shows the headcount ratios of different indicators of MPI at different levels, which can reveal structural differences in urban and rural poverty."),
          shiny::tags$hr()
        )
      ),
      shiny::sidebarLayout(
        sidebarPanel = shiny::sidebarPanel(
         
            
            add_pickerinput_shinywidget(inputID = ns("hri_measures"),
                                        label = "Measures",
                                        choices = c("afg","ind","chn"),
                                        selected = "afg"),
            add_pickerinput_shinywidget(inputID = ns("hri_area"),
                                        label = "Areas",
                                        choices = c("National","Urban","Rural"),
                                        selected = "National"),
            add_pickerinput_shinywidget(inputID = ns("hri_indicators"),
                                        label = "indicators",
                                        choices = c("assests","Sanitation","Electricity"),
                                        selected = "Electricity"),
            shiny::actionButton(inputId = ns("hrisubmit"),
                                label = "Show Results")
            
          ,width = 2
        ),
        mainPanel = shiny::mainPanel(
          shiny::tabsetPanel(
            shiny::tabPanel("Spatial Representation",
                            shiny::plotOutput(ns("map"),height = "500px")),
            shiny::tabPanel("Column Chart",
                            shiny::plotOutput(ns("bar"),height = "500px")),
            shiny::tabPanel("Table",
                            DT::DTOutput(ns("table"),height = "500px"))
          ),width = 10
        ))
    )
  )
}
    
#' headcount_indicators Server Functions
#'
#' @noRd 
mod_headcount_indicators_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$table <- DT::renderDataTable({
      shinipsum::random_DT(nrow = 5,ncol = 3)
    })
    
    output$bar <- shiny::renderPlot({
      shinipsum::random_ggplot()
    })
    
    output$map <- shiny::renderPlot({
      shinipsum::random_ggplot()
    })
 
  })
}
    
## To be copied in the UI
# mod_headcount_indicators_ui("headcount_indicators_ui_1")
    
## To be copied in the server
# mod_headcount_indicators_server("headcount_indicators_ui_1")
