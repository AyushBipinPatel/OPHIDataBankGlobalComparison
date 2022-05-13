## code to prepare `raw_2021_release` dataset goes here
## The 2021 release of the GMPI data is stored in the file "GMPI2021_dataviz.csv"

### The purpose of this script is to produce an internal data set for this package/app.
### This data set can be derived/subset from the above mentioned csv, specifically for the global comparisions app


# library -----------------------------------------------------------------

library(dplyr)


# read in the csv ----------------------------------------------------------

vroom::vroom(here::here("data-raw/GMPI2021_dataviz.csv")) -> raw_2021_release


# derive the necessary data -----------------------------------------------
rbind(
  raw_2021_release %>% 
    filter(measure %in% c("actb","hdk","H","A",
                          "M0","pctb","sev","hd","vuln")&
             (k == 33|is.na(k))&
             area_lab %in% c("Urban","Rural","National")) %>% 
    select(b,ccty,cty_lab,area_lab,ind_lab,misind_lab,
           measure_lab,survey,year,w_region,measure),
  raw_2021_release %>% 
    filter(measure == "H_190") %>% 
    select(b,ccty,cty_lab,area_lab,ind_lab,misind_lab,
           measure_lab,survey,year,w_region,measure)
)-> raw_2021_release


raw_2021_release %>% 
  mutate(
    ccty = as.factor(ccty),
    cty_lab = as.factor(cty_lab),
    area_lab = as.factor(area_lab),
    ind_lab = forcats::fct_relevel(as.factor(ind_lab),
                                   c("Nutrition","Child mortality","Years of schooling","School attendance","Cooking fuel","Sanitation","Drinking water","Electricity","Housing","Assets")),
    measure_lab = as.factor(ifelse(measure == "H_190","1.90$ a day",measure_lab)),
    survey = as.factor(survey),
    year = as.factor(year),
    w_region = as.factor(w_region),
    b = ifelse(measure == "M0", 
               round(b,3),
               round(b,2))
  ) -> raw_2021_release




## Th layout has to be saved as well for internal use

#map_layout_data <- highcharter::download_map_data("custom/world-robinson-highres.js")

###### why are the above lines commented?? Well, is download_map_data is set to FALSE, hcmap automatically makes a world map, that's what we need, so no need to download map data

## Caption for every chart is here as the following object

cap_charts = "Source: Ayush Patel, Researcher at OPHI, based on <b>Alkire,S., Kanagaratnam,U. and Suppa,N. (2021.'The Global Multidimensional Poverty Index (MPI) 2021', OPHI Methodological Note 51, Oxford Poverty and Human Development Initiative, University of Oxford.</b>"

# Save objects

usethis::use_data(raw_2021_release, cap_charts, overwrite = TRUE,internal = TRUE)