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
                                        choices = c(
                                          "Absolute contribution" = "Absolute contribution",
                                          "Percentage (%) Contribution" = "Relative contribution"
                                          ),
                                        selected = "Absolute contribution"),
            add_pickerinput_shinywidget(inputID = ns("ci_area"),
                                        label = "Areas",
                                        choices = c("National","Urban","Rural"),
                                        selected = "National"),
            shiny::actionButton(inputId = ns("cisubmit"),
                                label = "Apply Changes"),
            width = 2
        ),
        mainPanel = shiny::mainPanel(
          shiny::tabsetPanel(
            
            shiny::tabPanel("Chart",
                            highcharter::highchartOutput(ns("bar"),width = "100%",
                                                         height = "800px" )),
            shiny::tabPanel("Table",
                            DT::DTOutput(ns("table"),height = "800px"))
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
    
    # get reactive inputs
    
    sel_area <- shiny::eventReactive(input$cisubmit,{
      input$ci_area
    }, ignoreNULL = F)
    
    sel_measures <- shiny::eventReactive(input$cisubmit,{
      input$ci_measures
    }, ignoreNULL = F)
    
    
    # get reactive data
    
    ci_data <- shiny::eventReactive(input$cisubmit,{
      
      subset_data_according_section(section = "contri_indicators") %>%
        dplyr::filter(measure_lab == input$ci_measures &
                        area_lab ==  input$ci_area )
    }, ignoreNULL = F)
    
    output$table <- DT::renderDataTable({
      DT::datatable(
        ci_data() %>% 
          tidyr::pivot_wider(names_from = ind_lab,values_from = b) %>% 
          dplyr::select(-c("misind_lab","measure","measure_lab","ccty","area_lab"))%>% 
          dplyr::arrange(cty_lab) %>% 
          dplyr::select(cty_lab:w_region,Nutrition,dplyr::everything()),
        colnames = c("Country","Survey","Survey Year","World Region",levels(raw_2021_release$ind_lab)),
        filter = list(position = 'top', clear = FALSE),
        options = list(
          columnDefs = list(list(className = 'dt-center', targets = "_all"))
        ),
        caption = htmltools::tags$caption(
          style = 'caption-side: top; text-align: center;', 
          htmltools::em(paste(sel_measures(),"of poverty at", sel_area(),"level"))
        )
      )
    })
    
    output$bar <- highcharter::renderHighchart({
      
      col_chart_title <- switch (
        sel_measures(),
        "Absolute contribution" = "Absolute contribution",
        "Relative contribution" = "Percentage (%) Contribution"
      )
      
      hch_stacked_column_chart(data_passed = ci_data() %>%
                                 tidyr::pivot_wider(names_from = ind_lab,values_from = b,values_fill = 0) %>%
                                 tidyr::pivot_longer(cols = unique(raw_2021_release$ind_lab)[-10],names_to = "ind_lab",values_to = "b") %>% # check is 10 is NA, if not change to appropriate number
                                 dplyr::group_by(cty_lab) %>% 
                                 dplyr::mutate(
                                   sumind = sum(b,na.rm = T)
                                 ) %>% 
                                 dplyr::ungroup() %>% 
                                 dplyr::mutate(
                                   cty_lab = forcats::fct_reorder(cty_lab,sumind),
                                   ind_lab = forcats::fct_relevel(as.factor(ind_lab),
                                                                  c("Nutrition","Child mortality","Years of schooling","School attendance","Cooking fuel","Sanitation","Drinking water","Electricity","Housing","Assets"))
                                 ) %>% 
                                 dplyr::arrange(dplyr::desc(cty_lab)),
                               groups = "ind_lab",
                               xaxis = "cty_lab",
                               yaxis = "b",
                               stack = ifelse(col_chart_title == "Absolute contribution",
                                              "normal","percent"),
                               title = paste(col_chart_title," of MPI Indicators at the ",sel_area()," level"),
                               flname = paste(col_chart_title," of MPI Indicators at the ",sel_area()," level"),
                               tooltip = paste("Absolute contributon of {point.ind_lab} : {point.y}<br>Percentage Contribution of {point.ind_lab} : {point.percentage:.1f}%<br>Survey : {point.survey} <br>Survey Year : {point.year}"),
                               xtitle = NULL,
                               ytitle = col_chart_title
                               )
    })
 
  })
}
    
## To be copied in the UI
# mod_contribution_indicators_ui("contribution_indicators_ui_1")
    
## To be copied in the server
# mod_contribution_indicators_server("contribution_indicators_ui_1")
