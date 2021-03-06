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
                                        choices = c("Censored headcount ratio",
                                                    "Uncensored headcount ratio"),
                                        selected = "Censored headcount ratio"),
            add_pickerinput_shinywidget(inputID = ns("hri_area"),
                                        label = "Areas",
                                        choices = c("National","Urban","Rural"),
                                        selected = "National"),
            add_pickerinput_shinywidget(inputID = ns("hri_indicators"),
                                        label = "Indicators",
                                        choices = c("Nutrition",
                                                    "Child mortality",
                                                    "Years of schooling",
                                                    "School attendance",
                                                    "Cooking fuel",
                                                    "Sanitation",
                                                    "Drinking water",
                                                    "Electricity",
                                                    "Housing",
                                                    "Assets"),
                                        selected = "Nutrition"),
            shiny::actionButton(inputId = ns("hrisubmit"),
                                label = "Apply Changes"),
            shiny::conditionalPanel(
              condition = "input.mod_hri_tubbs == 'Chart'",
              ns = ns,
              shiny::tags$hr(),
              shiny::tags$h6("Additional Controls for Column Chart"),
              add_pickerinput_shinywidget(
                inputID = ns("hricolumnchoice"),
                label = "Choose countries to compare",
                choices = levels(raw_2021_release$cty_lab),
                selected = NULL,
                multiple = T
              ),
              
              shiny::actionButton(inputId = ns("hricolumnreset"),
                                  "Reset Chart")
            ),
            
          width = 2
        ),
        mainPanel = shiny::mainPanel(
          shiny::tabsetPanel(id = ns("mod_hri_tubbs"),
            shiny::tabPanel("Map",
                            highcharter::highchartOutput(ns("map"),width = "100%",
                                                         height = "800px")),
            shiny::tabPanel("Chart",
                            highcharter::highchartOutput(ns("bar"), width = "100%", 
                                                         height = "800px")),
            shiny::tabPanel("Table",
                            DT::DTOutput(ns("table"),height = "800px"))
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
    
    # get reactive inputs
    
    sel_col_countries <- reactive(input$hricolumnchoice)
    
    sel_col_countries_len <- reactive({
      length(input$hricolumnchoice)
    })
    
    
    shiny::observeEvent(input$hricolumnreset,{
      shinyWidgets::updatePickerInput(inputId = "hricolumnchoice",
                                      choices = levels(raw_2021_release$cty_lab),
                                      session = session)
    }
    )
    
    sel_area <- shiny::eventReactive(input$hrisubmit,{
      input$hri_area
    },ignoreNULL = F)
    
    sel_measures <- shiny::eventReactive(input$hrisubmit,{
      input$hri_measures
    },ignoreNULL = F)
    
    sel_indicators <- shiny::eventReactive(input$hrisubmit,{
      input$hri_indicators
    },ignoreNULL = F)
    
    # get reactive data
    
    hri_data <- shiny::eventReactive(input$hrisubmit,{
      
      subset_data_according_section(section = "h_indicators") %>% 
        dplyr::filter(measure_lab == input$hri_measures &
                        area_lab ==  input$hri_area & ind_lab == input$hri_indicators)
    },ignoreNULL = F)
    
    output$table <- DT::renderDataTable({
      DT::datatable(
        hri_data() %>% 
          dplyr::select(-c("measure","misind_lab","ccty","measure_lab"))%>% 
          dplyr::arrange(cty_lab),
        colnames = c(sel_measures(),"Country","Area","Indicator","Survey","Survey Year","World Region"),
        filter = list(position = 'top', clear = FALSE),
        options = list(
          columnDefs = list(list(className = 'dt-center', targets = "_all"))
        ),
        caption = htmltools::tags$caption(
          style = 'caption-side: top; text-align: center;', 
          htmltools::em(paste(sel_measures(),"of", sel_indicators(),
                              "at",sel_area(),"level"))
        )
      )
    })
    
    output$bar <- highcharter::renderHighchart({
      
      # prep data for chart
      if(is.null(sel_col_countries())){
        chart_data <- hri_data() %>%
          dplyr::arrange(dplyr::desc(b))
      }else{
        chart_data <- hri_data() %>%
          dplyr::filter(cty_lab %in% sel_col_countries()) %>% 
          dplyr::arrange(dplyr::desc(b))
      }
      
      
      hch_simple_column_chart(chart_data,
                              x_axis = "cty_lab",
                              y_axis = "b",
                              title = paste0(sel_measures()," (%) of ",sel_indicators()," at ",sel_area()," Level"),
                              flname = paste0(sel_measures()," (%) of ",sel_indicators()," at ",sel_area()," Level"), # same as the chart title as it would make sense to save a chart by its title 
                              tooltip = paste("Measure : ",sel_measures(),"<br>Measure Value : {point.y}<br>Survey : {point.survey} <br>Survey Year : {point.year}"),
                              xtitle = NULL,
                              ytitle = sel_measures())
    })
    
    output$map <- highcharter::renderHighchart({
      hch_choropleth(
        passed_data = hri_data(),
        catch_sel_measure = paste(sel_measures()," of", sel_indicators()),
        catch_sel_area = sel_area()
      )
    })
 
  })
}
    
## To be copied in the UI
# mod_headcount_indicators_ui("headcount_indicators_ui_1")
    
## To be copied in the server
# mod_headcount_indicators_server("headcount_indicators_ui_1")
