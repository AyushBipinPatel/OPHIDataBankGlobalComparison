#' aggregate_measures UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_aggregate_measures_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    shiny::fluidPage(
      
       shiny::tags$div(
         h1(shiny::tags$b(shiny::tags$span(style = "color:#7c1419","Aggregate Measures: MPI,H and A, Vulnerability, and Severe Poverty"))),
         tags$div(
           shiny::tags$p("In the global MPI, a person is identified as multidimensionally poor or MPI poor if they are deprived in at least one third of the weighted MPI indicators. In other words, a person is MPI poor if the person’s weighted deprivation score is equal to or higher than the poverty cutoff of 33.33%. Following the AF methodology, the MPI is calculated by multiplying the incidence of poverty (H) and the average intensity of poverty (A). More specifically, H is the proportion of the population that is multidimensionally poor, while A is the average proportion of dimensions in which poor people are deprived. So, MPI = H × A, reflecting both the share of people in poverty and the degree to which they are deprived."),
           shiny::tags$br(),
           shiny::tags$p("A headcount ratio is also estimated for two other ranges of poverty cutoffs. A person is identified as vulnerable to poverty if they are deprived in 20–33.32% of the weighted indicators. Concurrently, a person is identified as living in severe poverty if they are deprived in 50–100% of the weighted indicators. Below is a graphical and spatial illustration of the Global MPI data."),
           shiny::tags$hr()
         )
       ),
      shiny::sidebarLayout(
        sidebarPanel = shiny::sidebarPanel(
          
          
          add_pickerinput_shinywidget(inputID = ns("agmeasures"),
                                      label = "Choose a Measure",
                                      choices = c(
                                        "Headcount ratio",
                                        "Intensity",
                                        "MPI",
                                        "Severe Poor",
                                        "Vulnerable"
                                      ),
                                      selected = "MPI"),
          add_pickerinput_shinywidget(inputID = ns("agarea"),
                                      label = "Choose an Area",
                                      choices = c("National","Urban","Rural"),
                                      selected = "National"),
          
          shiny::actionButton(inputId = ns("agsubmit"),
                              label = "Show Results"),
          width = 2
        ),
        mainPanel = shiny::mainPanel(
          shiny::tabsetPanel(
            shiny::tabPanel("Spatial Representation",
                            shiny::plotOutput(ns("map"),height = "500px")),
            shiny::tabPanel("Column Chart",
                            shiny::plotOutput(ns("bar"),height = "500px")),
            shiny::tabPanel("Table",
                            DT::DTOutput(ns("table"),height = "500px")
            )
          ),width = 10
        ))
    )
 
  )
}
    
#' aggregate_measures Server Functions
#'
#' @noRd 
mod_aggregate_measures_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    agg_measures_data <- shiny::eventReactive(input$agsubmit,{
      
      subset_data_according_section(section = "agg_measures") %>% 
        dplyr::filter(measure_lab == input$agmeasures &
                        area_lab ==  input$agarea)
    })
    
    
    output$table <- DT::renderDT({
      
      agg_measures_data()
    })
    
    
    # output$bar <- shiny::renderPlot({
    #   
    # })
    # 
    # output$map <- shiny::renderPlot({
    #   
    # })
 
  })
}
    
## To be copied in the UI
# mod_aggregate_measures_ui("aggregate_measures_ui_1")
    
## To be copied in the server
# mod_aggregate_measures_server("aggregate_measures_ui_1")
