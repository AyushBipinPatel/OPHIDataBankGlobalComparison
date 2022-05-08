#' hch_simple_column_chart 
#'
#' @param data_passed expects a data frame object using which a chart is created
#' @param x_axis A string,The name of the column from data frame passed to be used on x axis
#' @param y_axis A string,The name of the column from data frame passed to be used on y axis
#' @param title A string, desired title for the chart.
#' @param flname A string, desired name of the file when user choose to save the chart
#' @param tooltip A string, to show when tooltip of the mouse interacts with the chart area
#' @param xtitle A string, to provide x axis title
#' @param ytitle A string, to provide y axis title
#'
#' @description A fct function to generate simple column chart using the highcharter library
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

hch_simple_column_chart <- function(data_passed,x_axis,y_axis,title,flname,tooltip,xtitle,ytitle){
  
  highcharter::hchart(data_passed,
         "column",
         highcharter::hcaes(x = .data[[x_axis]], y = .data[[y_axis]]),
         color = "#c7a52c"
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
                          scrollbar = list(enabled = T),minrange = 15) %>% 
    highcharter::hc_yAxis(title = list(text = ytitle)) %>% 
    highcharter::hc_tooltip(
      pointFormat = tooltip
    )%>% 
    highcharter::hc_caption(text = cap_charts)
  
  
  
}