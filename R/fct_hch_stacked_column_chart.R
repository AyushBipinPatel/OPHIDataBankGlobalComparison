#' hch_stacked_column_chart 
#'
#' @param data_passed A data frame using which the chart has to be created
#' @param groups A string, having the name of the column in the data frame to be used a group
#' @param xaxis A string, providing a column name to map with the x axis
#' @param yaxis A string, providing a column name to map with the x axis
#' @param stack A string, either "normal" or "percent" to create a stacked column chart with absolute values or stacked column chart with relative/percentage values
#' @param title A string, desired title for the chart 
#' @param tooltip A string, providing what you need to be displayed on mouse hover
#' @param xtitle A string, desired x axis title
#' @param ytitle A string, desired y axis title
#' @param flname A string, provides the name of the file to save the chart/data
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

hch_stacked_column_chart <- function(data_passed,groups,xaxis,yaxis,stack,title,
                                     tooltip,xtitle,ytitle,flname){
  
  highcharter::hchart(object = data_passed, type = "column",
                      highcharter::hcaes(x = .data[[xaxis]],
                                         y = .data[[yaxis]],
                                         group = .data[[groups]])) %>% 
    highcharter::hc_plotOptions(
      column = list(stacking = stack)
    ) %>% 
    highcharter::hc_exporting(
      enabled = TRUE, 
      buttons = list(
        contextButton = list(
          menuItems = c("viewFullscreen", "printChart", "separator", "downloadPNG", 
                        "downloadJPEG", "downloadPDF", "downloadSVG","separator", "downloadCSV")
        )
      ),
      filename = flname
    ) %>% 
    highcharter::hc_title(
      text = title,
      margin = 20,
      align = "left",
      style = list(color = "#22A884", useHTML = TRUE)
    )  %>% 
    highcharter::hc_chart(zoomType = "x") %>% 
    highcharter::hc_xAxis(title = list(text = xtitle),
                          scrollbar = list(enabled = T),
                          minrange = 5,
                          labels = list(
                            rotation = -90,
                            step = 1
                          )) %>% 
    highcharter::hc_yAxis(title = list(text = ytitle)) %>% 
    highcharter::hc_tooltip(
      pointFormat = tooltip
    ) %>% 
    highcharter::hc_colors(
      c("#962c20","#632524","#c5a9ab","#a58580","#adc6d6","#7e9eb3","#5d8099","#416680","#154b66","#003650")
    )%>% 
    highcharter::hc_caption(text = cap_charts)
  
  
}