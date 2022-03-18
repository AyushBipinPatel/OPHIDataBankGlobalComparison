#' headcount_poverty UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_headcount_poverty_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::fluidPage(
      
        shiny::tags$div(
          h1(shiny::tags$b(shiny::tags$span(style = "color:#7c1419","Headcount of Poverty: MPI and $1.90/day"))),
          tags$div(
            shiny::tags$p("The following visualisation shows the percentage of people who are MPI poor and severely poor in the countries analyzed. The percentage of people who are MPI poor is shown in beige. The height at each dot denotes the percentage of people who are monetary poor according to the $1.90 a day poverty line in each country. The monetary poverty statistics are taken from the year closest to the year of the survey used to calculate the MPI. In cases where a survey was conducted over two calendar years, the later period is taken as the reference year."),
            shiny::tags$hr()
          )
        ),
        shiny::tabsetPanel(
          shiny::tabPanel("Column Chart",
                          shiny::plotOutput(ns("bar"),height = "500px")),
          shiny::tabPanel("Data Table",
                          DT::DTOutput(ns("table"),height = "500px"))
        )
    )
 
  )
}
    
#' headcount_poverty Server Functions
#'
#' @noRd 
mod_headcount_poverty_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    output$table <- DT::renderDataTable({
      
      DT::datatable(
        raw_2021_release %>% 
          dplyr::filter(measure %in% c("H","H_190","sev") & 
                          (is.na(area_lab) |area_lab == "National")) %>% 
          dplyr::select(-c("area_lab","ind_lab","misind_lab","measure")) %>% 
          dplyr::arrange(ccty) %>% 
          tidyr::fill(cty_lab,.direction = "down"),
        colnames = c("Value","ISO","Country","Measure","Survey","Survey Year","World Region"),
        filter = list(position = 'top', clear = FALSE),
        options = list(
          columnDefs = list(list(className = 'dt-center', targets = "_all"))
        )
      )
      
    })
    
    output$bar <- shiny::renderPlot({
      shinipsum::random_ggplot()
    })
 
  })
}
    
## To be copied in the UI
# mod_headcount_poverty_ui("headcount_poverty_ui_1")
    
## To be copied in the server
# mod_headcount_poverty_server("headcount_poverty_ui_1")
