#' subset_data_according_section 
#'
#' @param section expects one of the four strings: "agg_measures","h_poverty","h_indicators","contri_indicators"
#'
#' @description A fct function to subset data as needed by every section of the global comparisions app
#'
#' @return A data frame is returned by this function, appropriate to create visualization of respective section selected by the user
#'
#' @noRd

subset_data_according_section <- function(section){
  
  # make sure section param is provided appropriate string
  
  if(!(section %in% c("agg_measures","h_poverty","h_indicators","contri_indicators"))){
    stop('"section" param takes on of these strings:"agg_measures","h_poverty","h_indicators","contri_indicators"')
  }
  
  # subset and return
  
  filtered_data <- switch (section,
                           agg_measures = raw_2021_release %>% dplyr::filter(measure %in% c("M0","H","A","sev","vuln")),
                           h_poverty = raw_2021_release %>% dplyr::filter(measure %in% c("H_190","H") & (area_lab == "National"|is.na(area_lab))),
                           h_indicators = raw_2021_release %>% dplyr::filter(measure %in% c("hd","hdk")),
                           contri_indicators = raw_2021_release %>% dplyr::filter(measure %in% c("actb","pctb"))
  )
  
  return(filtered_data)
}