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
                                        "H - Headcount ratio of poverty (%)" = "Headcount ratio",
                                        "A - Intensity of Poverty (%)" = "Intensity",
                                        "MPI - Multidimensional Poverty Index (range 0 to 1)" = "MPI",
                                        "Headcount ratio of Severe Poverty  (K>50%) (%)" = "Severe Poor",
                                        "Vulnerability to poverty (20% < K <33.32%) (%)" = "Vulnerable"
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
                          highcharter::highchartOutput(ns("bar"), width = "100%", 
                                            height = "500px")),
            shiny::tabPanel("Data Table",
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
    
    sel_measure <- shiny::eventReactive(input$agsubmit,{input$agmeasures})
    sel_area <- shiny::eventReactive(input$agsubmit,{input$agarea})
    
    
    
    agg_measures_data <- shiny::eventReactive(input$agsubmit,{
      
      subset_data_according_section(section = "agg_measures") %>% 
        dplyr::filter(measure_lab == input$agmeasures &
                        area_lab ==  input$agarea)
    })
    
    
    output$table <- DT::renderDT({
      
      DT::datatable(
        agg_measures_data() %>% 
          dplyr::select(-c("ind_lab","misind_lab","measure")),
        colnames = c("Value","ISO","Country","Area","Measure","Survey","Survey Year","World Region"),
        filter = list(position = 'top', clear = FALSE),
        options = list(
          columnDefs = list(list(className = 'dt-center', targets = "_all"))
        )
                    )
      
    })
    
    
    output$bar <- highcharter::renderHighchart({
      
      # create column title first
      col_chart_title <- switch (
        sel_measure(),
        "Headcount ratio" = "H - Headcount ratio of poverty (%)" ,
        "Intensity" = "A - Intensity of Poverty (%)" ,
        "MPI" = "MPI - Multidimensional Poverty Index (range 0 to 1)" ,
        "Severe Poor" = "Headcount ratio of Severe Poverty  (K>50%) (%)" ,
        "Vulnerable" = "Vulnerability to poverty (20% < K <33.32%) (%)"
      )
      
      # now the chart function
      
      hch_simple_column_chart(agg_measures_data() %>% dplyr::arrange(dplyr::desc(b)),
                              x_axis = "cty_lab",
                              y_axis = "b",
                              title = paste0(col_chart_title," at ",sel_area()," Level"),
                              flname = paste0(col_chart_title," at ",sel_area()," Level"), # same as the chart title as it would make sense to save a chart by its title 
                              tooltip = paste("Measure : ",col_chart_title,"<br>Measure Value : {point.y}<br>Survey : {point.survey} <br>Survey Year : {point.year}"),
                              xtitle = NULL,
                              ytitle = col_chart_title)
    })
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
