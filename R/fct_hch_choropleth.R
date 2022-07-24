#' hch_choropleth 
#'
#' @param passed_data The data frame to use to create the map
#' @param catch_sel_measure pass the measure selected by the user to feed in the tooltip
#' @param catch_sel_area pass the area selected by the user to feed in the tooltip
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

hch_choropleth <- function(passed_data,catch_sel_measure,catch_sel_area){
  
# create a data class for the hc_* function  
  
  dta_class <- dplyr::tibble(
    name = passed_data %>% 
      dplyr::mutate(
        discrete_grps = ggplot2::cut_number(x = b,n = 7)
      ) %>% 
      dplyr::pull(discrete_grps) %>% 
      levels(),
    from = c(1:7),
    to= c(2:8)
  ) %>% 
    highcharter::list_parse()
  
  
# The map  
   
  highcharter::hcmap(
                     download_map_data = FALSE, #setting this to false does avoid the url call and download with every input change, however, it also generates an empty plot for the first set of inputs, submitting inputs again shows the plot. time difference between setting it to T or F is ~ same
                     data = passed_data %>%
                       dplyr::mutate(
                         discrete_grps = ggplot2::cut_number(x = b,n = 7),
                         discrete_grps = as.numeric(discrete_grps)
                       ) ,
                     value = "discrete_grps",
                     name= NULL,
                     joinBy =  c("iso-a3","ccty"),
                     nullColor = "#DDDDDD",
                     borderColor = "#FFFFFF") %>%
    highcharter::hc_colorAxis(
      dataClassColor = "category",
      dataClasses = dta_class
    ) %>% 
    highcharter::hc_tooltip(
      pointFormat = paste(catch_sel_measure,": {point.b}<br>Survey : {point.survey}<br>Survey Year: {point.year}"),
      headerFormat = paste("<b>{point.key} at ",catch_sel_area, " Level</b><br>")
    ) %>% 
    highcharter::hc_colors(
      colors = c("#b0d9cb",
                 "#abcd72",
                 "#e2d200",
                 "#fdd262",
                 "#f18b00",
                 "#cb1724",
                 "#5b1a18")
      
    ) %>% 
    highcharter::hc_exporting(
      enabled = TRUE, 
      buttons = list(
        contextButton = list(
          menuItems = c("viewFullscreen", "printChart", "separator", "downloadPNG", 
                        "downloadJPEG", "downloadPDF", "downloadSVG","separator", "downloadCSV")
        )
      ),
      filename = paste(catch_sel_measure,": {point.b}<br>Survey : {point.survey}<br>Survey Year: {point.year}")
    ) %>% 
    highcharter::hc_mapNavigation(
      enabled = TRUE,
      enableMouseWheelZoom = TRUE,
      enableDoubleClickZoom = TRUE
    ) %>% 
    highcharter::hc_caption(text = cap_charts) %>% 
    highcharter::hc_title(
      text = paste(catch_sel_measure,"at", catch_sel_area, "Level."),
      margin = 20,
      align = "left",
      style = list(color = "#22A884", useHTML = TRUE)
    ) 
  
}