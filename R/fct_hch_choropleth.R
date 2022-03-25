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
  
  highcharter::hcmap(mapData = map_layout_data,
                     data = passed_data %>% 
                       dplyr::mutate(
                         discrete_grps = ggplot2::cut_number(x = b,n = 7),
                         discrete_grps = as.numeric(discrete_grps)
                       ) ,
                     value = "discrete_grps",
                     name= NULL,
                     joinBy =  c("iso-a3","ccty")) %>% 
    highcharter::hc_colorAxis(
      dataClassColor = "category",
      dataClasses = dta_class
    ) %>% 
    highcharter::hc_tooltip(
      pointFormat = paste(catch_sel_measure,": {point.b}<br>Survey : {point.survey}<br>Survey Year: {point.year}"),
      headerFormat = paste("<b>{point.key} at ",catch_sel_area, " Level</b><br>")
    ) %>% 
    highcharter::hc_colors(
      colors = c("#d3d3d3","#898999","#ffc051",
                 "#e09500","#c6431f","#a62420","#5e1211")
    ) %>% 
    highcharter::hc_exporting(
      enabled = TRUE, 
      filename = paste(catch_sel_measure,": {point.b}<br>Survey : {point.survey}<br>Survey Year: {point.year}")
    ) %>% 
    highcharter::hc_mapNavigation(
      enabled = TRUE,
      enableMouseWheelZoom = TRUE,
      enableDoubleClickZoom = TRUE
    )
  
}