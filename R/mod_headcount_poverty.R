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
          shiny::tabPanel("Chart",
                          highcharter::highchartOutput(ns("bar"),
                                                       width = "100%",height = "800px")),
          shiny::tabPanel("Table",
                          DT::DTOutput(ns("table"),height = "800px"))
        ),
        shiny::tags$br(),
        shiny::fluidRow(
          shiny::column(width = 6,
                        highcharter::highchartOutput(ns("scatter1"),
                                                     width = "100%",height = "800px")            
                        ),
          shiny::column(width = 6,
                        highcharter::highchartOutput(ns("scatter2"),
                                                     width = "100%",height = "800px")   
                        )
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
    
    data_hp <- raw_2021_release %>% 
      dplyr::filter(measure %in% c("H","H_190","sev") & 
                      (is.na(area_lab) |area_lab == "National")) %>% 
      dplyr::select(-c("area_lab","ind_lab","misind_lab")) %>% 
      dplyr::arrange(ccty) %>% 
      tidyr::fill(cty_lab,.direction = "down")
    
    output$table <- DT::renderDataTable({
      
      DT::datatable(
        data_hp %>% 
          dplyr::select(-measure) %>% 
          tidyr::fill(cty_lab,.direction = "down") %>% 
          dplyr::select(-c("ccty")) %>% 
          dplyr::arrange(cty_lab),
        colnames = c("Measure Value","Country","Measure","Survey","Survey Year","World Region"),
        filter = list(position = 'top', clear = FALSE),
        options = list(
          columnDefs = list(list(className = 'dt-center', targets = "_all"))
        ),
        caption = htmltools::tags$caption(
          style = 'caption-side: top; text-align: center;', 
          htmltools::em("Measures of Poverty: MPI headcount and $1.90 a day")
        )
      )
      
    })
    
    output$bar <- highcharter::renderHighchart({
      
      
      highcharter::hchart(object = data_hp %>% 
                            dplyr::select(-measure) %>% 
                            dplyr::filter(measure_lab!= "1.90$ a day") %>% 
                            tidyr::pivot_wider(names_from = measure_lab,values_from = b) %>% 
                            dplyr::mutate(
                              poor_not_severe = `Headcount ratio` - `Severe Poor`
                            ) %>% 
                            tidyr::pivot_longer(cols = c("poor_not_severe","Severe Poor"),
                                                names_to = "measure_lab",
                                                values_to = "b") %>% 
                            dplyr::mutate(
                              cty_lab = forcats::fct_reorder(as.factor(cty_lab),
                                                                       `Headcount ratio`)
                            ) %>% 
                            dplyr::arrange(dplyr::desc(cty_lab)),
                          type = "column", 
                          highcharter::hcaes(x = cty_lab,
                                             y = b,
                                             group = measure_lab)) %>% 
        highcharter::hc_plotOptions(
          series = list(
            stacking = "normal"
          )
        ) %>% 
        highcharter::hc_add_series(data = data_hp %>% 
                                     dplyr::filter(measure_lab == "1.90$ a day"),
                                   type = "point",
                                   highcharter::hcaes(x = cty_lab,
                                                      y = b),
                                   name = "1.90$ a day") %>% 
        highcharter::hc_exporting(
        enabled = TRUE,
        buttons = list(
          contextButton = list(
            menuItems = c("viewFullscreen", "printChart", "separator", "downloadPNG", 
                          "downloadJPEG", "downloadPDF", "downloadSVG","separator", "downloadCSV")
          )
        ),
        filename = "Headcount Ratios: Multidimensional Poverty and $1.90 a day at the country level"
                         ) %>% 
        highcharter::hc_title(
          text = "Headcount Ratios: Multidimensional Pverty and $1.90 a day at the country level",
          margin = 20,
          align = "left",
          style = list(color = "#22A884", useHTML = TRUE)
        )  %>% 
        highcharter::hc_chart(zoomType = "x") %>% 
        highcharter::hc_xAxis(title = list(text = NULL),
                              scrollbar = list(enabled = T),
                              min = 1, # These two things are to control number of bars in the container, scroll bar can work 
                              max = 50, # These two things are to control number of bars in the container, scroll bar can work
                              labels = list(
                                rotation = -90,
                                step = 1
                              )) %>% 
        highcharter::hc_yAxis(title = list(text = "% of population")) %>% 
        highcharter::hc_tooltip(
          formatter = htmlwidgets::JS(
            'function(){
              if(this.series.name == "1.90$ a day"){
              return this.point.cty_lab + "<br>" + this.series.name + ":" + this.point.y + "<br>Source : World bank";
              }else{
              if(this.point.measure_lab == "poor_not_severe"){
              return this.point.cty_lab + "<br>MPI Poor not in severe poverty :" + this.point.y + "<br>Survey : " + this.point.survey + "<br>Survey year : " + this.point.year + "<br>Total MPI Poor : " + this.point.stackTotal;
              }else{
              return this.point.cty_lab + "<br>MPI Poor in severe Poverty (K>50) :" + this.point.y + "<br>Survey : " + this.point.survey + "<br>Survey year : " + this.point.year + "<br>Total MPI Poor : " + this.point.stackTotal;
              }}
            }'
          )
        ) %>% 
        highcharter::hc_colors(
          c("#c14c54", "#7c1419","#191919")
        ) %>% 
        highcharter::hc_caption(text = cap_charts) %>% 
        highcharter::hc_legend(
          labelFormatter = htmlwidgets::JS(
            "function () {
             if(this.name == '1.90$ a day'){
              return 'poor as per 1.90$ a day measure';
             }else{
             if(this.name == 'poor_not_severe'){
              return 'MPI poor but not in severe poverty';
             }else{
              return 'MPI Poor and in severe poverty';
             }
             }
        }"
          ),
        symbolRadius = htmlwidgets::JS("function(){
                   if(this.series.name == '1.90$ a day'){
                   return 0;
                   }else{
                   return 4;
                   }
        }")
        )
      
      
    })
 
  
  
    output$scatter1 <- highcharter::renderHighchart({
    highcharter::hchart(
      object = dplyr::left_join(
        data_hp %>% 
          dplyr::filter(measure %in% c("H")) %>% 
          dplyr::rename("H" = "b") %>% 
          dplyr::select(-c( measure_lab,measure)),
        data_hp %>% 
          dplyr::filter(measure %in% c("H_190")) %>% 
          dplyr::rename("H_190" = "b","year_190" = "year") %>% 
          dplyr::select(-c(w_region,cty_lab, measure_lab,
                           survey,measure)),
        by = c("ccty" = "ccty")
      ),
      type = "scatter",
      highcharter::hcaes(H, H_190)
    ) %>% 
      highcharter::hc_add_series(
        dashStyle = "ShortDash",
        data = list(
          list(
            x = 0,
            y = 0
          ),
          list(
            x = 100,
            y = 100
          )
        )
      ) %>% 
      highcharter::hc_exporting(
        enabled = TRUE,
        buttons = list(
          contextButton = list(
            menuItems = c("viewFullscreen", "printChart", "separator", "downloadPNG", 
                          "downloadJPEG", "downloadPDF", "downloadSVG","separator", "downloadCSV")
          )
        ),
        filename = "Headcount Ratios: Multidimensional Pverty and $1.90 a day at the country level"
      ) %>% 
      highcharter::hc_title(
        text = "Headcount Ratios: Multidimensional Pverty and $1.90 a day at the country level",
        margin = 20,
        align = "left",
        style = list(color = "#22A884", useHTML = TRUE)
      )  %>% 
      highcharter::hc_chart(zoomType = "yx") %>% 
      highcharter::hc_xAxis(title = list(text = "Population (%) Multidimensionally Poor"),
                            scrollbar = list(enabled = T)
                            ) %>% 
      highcharter::hc_yAxis(title = list(text = "Population (%) of poor as ber $1.90 a day ")) %>% 
      highcharter::hc_tooltip(
        formatter = htmlwidgets::JS(
          'function(){
              return this.point.cty_lab + 
          "<br> Percentage Poor as per $1.90 a day :" + this.point.y + 
          "<br> Percentage of MPI poor" + this.point.x  + 
          "<br>$1.90 a day Survey year : " + this.point.year_190 +
          "<br> MPI Survey: " + this.point.survey +
          "<br>MPI Survey year : " + this.point.year ;
          }'
        )
      ) %>% 
        highcharter::hc_plotOptions(
          scatter = list(
            dataLabels = list(
              enabled = TRUE,
              formatter = htmlwidgets::JS(
                'function(){
                return this.point.ccty;
                }'
              )
            )
          ),
          series = list(
            color = "#c6431f"
          )
        )
    
  })
  
    output$scatter2 <- highcharter::renderHighchart({
    highcharter::hchart(
      object = dplyr::left_join(
        data_hp %>% 
          dplyr::filter(measure %in% c("sev")) %>% 
          dplyr::rename("sev" = "b") %>% 
          dplyr::select(-c( measure_lab,measure)),
        data_hp %>% 
          dplyr::filter(measure %in% c("H_190")) %>% 
          dplyr::rename("H_190" = "b","year_190" = "year") %>% 
          dplyr::select(-c(w_region,cty_lab, measure_lab,
                           survey,measure)),
        by = c("ccty" = "ccty")
      ),
      type = "scatter",
      highcharter::hcaes(sev, H_190)
    ) %>% 
      highcharter::hc_add_series(
        dashStyle = "ShortDash",
        data = list(
          list(
            x = 0,
            y = 0
          ),
          list(
            x = 100,
            y = 100
          )
        )
      ) %>% 
      highcharter::hc_exporting(
        enabled = TRUE,
        buttons = list(
          contextButton = list(
            menuItems = c("viewFullscreen", "printChart", "separator", "downloadPNG", 
                          "downloadJPEG", "downloadPDF", "downloadSVG","separator", "downloadCSV")
          )
        ),
        filename = "Headcount Ratios: Multidimensional Pverty and $1.90 a day at the country level"
      ) %>% 
      highcharter::hc_title(
        text = "Headcount Ratios: Severe Multidimensional Poverty and $1.90 a day at the country level",
        margin = 20,
        align = "left",
        style = list(color = "#22A884", useHTML = TRUE)
      )  %>% 
      highcharter::hc_chart(zoomType = "yx") %>% 
      highcharter::hc_xAxis(title = list(text = "Population (%)in Severe Multidimensionally Poverty"),
                            scrollbar = list(enabled = T)
      ) %>% 
      highcharter::hc_yAxis(title = list(text = "Population (%) of poor as ber $1.90 a day ")) %>% 
      highcharter::hc_tooltip(
        formatter = htmlwidgets::JS(
          'function(){
              return this.point.cty_lab + 
          "<br> Percentage Poor as per $1.90 a day :" + this.point.y + 
          "<br> Percentage of Severe MPI poor (k > 50) :" + this.point.x  + 
          "<br>$1.90 a day Survey year : " + this.point.year_190 +
          "<br> MPI Survey: " + this.point.survey +
          "<br>MPI Survey year : " + this.point.year ;
          }'
        )
      )%>% 
        highcharter::hc_plotOptions(
          scatter = list(
            dataLabels = list(
              enabled = TRUE,
              formatter = htmlwidgets::JS(
                'function(){
                return this.point.ccty;
                }'
              )
            )
          ),
          series = list(
            color = "#c6431f"
          )
        )
    
  })
  
  })
  
}
    
## To be copied in the UI
# mod_headcount_poverty_ui("headcount_poverty_ui_1")
    
## To be copied in the server
# mod_headcount_poverty_server("headcount_poverty_ui_1")
