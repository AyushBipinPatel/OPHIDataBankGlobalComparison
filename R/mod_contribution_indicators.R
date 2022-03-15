#' contribution_indicators UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_contribution_indicators_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    shiny::fluidPage(
      
      shiny::tags$div(
        h1(shiny::tags$b(shiny::tags$span(style = "color:#7c1419","Contribution of Indicators"))),
        tags$div(
          shiny::tags$p("The censored headcount ratio shows the extent of deprivations among the poor but does not reflect the relative value of the indicators to overall poverty. Two indicators may have the same censored headcount ratios but different contributions to overall poverty, because the contribution depends both on the censored headcount ratio and on the weight assigned to each indicator. As such, a complementary analysis to the censored headcount ratio is the percentage contribution of each indicator to overall multidimensional poverty.The next visualisation allows two options: absolute contribution and percentage contribution of each indicator to national, rural and urban poverty. For percentage contributions, colors inside each bar denote the relative contribution of each indicator to the overall MPI, and all bars add up to 100%. For absolute contributions, the height of each bar adds up to the value of MPI. This enables an immediate visual comparison of the composition of poverty across areas."),
          shiny::tags$hr()
        )
      ),
      shiny::sidebarLayout(
        sidebarPanel = shiny::sidebarPanel(
          
            
            add_pickerinput_shinywidget(inputID = ns("ci_measures"),
                                        label = "Measures",
                                        choices = c("afg","ind","chn"),
                                        selected = "afg"),
            add_pickerinput_shinywidget(inputID = ns("ci_area"),
                                        label = "Areas",
                                        choices = c("National","Urban","Rural"),
                                        selected = "National"),
            shiny::actionButton(inputId = ns("cisubmit"),
                                label = "Show Results"),
            width = 2
        ),
        mainPanel = shiny::mainPanel(
          shiny::tabsetPanel(
            
            shiny::tabPanel("Column Chart",
                            shiny::plotOutput(ns("bar"),height = "500px")),
            shiny::tabPanel("Table",
                            DT::DTOutput(ns("table"),height = "500px"))
          ),width = 10
        ))
    )
 
  )
}
    
#' contribution_indicators Server Functions
#'
#' @noRd 
mod_contribution_indicators_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$table <- DT::renderDataTable({
      shinipsum::random_DT(nrow = 5,ncol = 3)
    })
    
    output$bar <- shiny::renderPlot({
      shinipsum::random_ggplot()
    })
 
  })
}
    
## To be copied in the UI
# mod_contribution_indicators_ui("contribution_indicators_ui_1")
    
## To be copied in the server
# mod_contribution_indicators_server("contribution_indicators_ui_1")
