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
    ind_lab = as.factor(ind_lab),
    measure_lab = as.factor(measure_lab),
    survey = as.factor(survey),
    year = as.factor(year),
    w_region = as.factor(w_region),
  ) -> raw_2021_release

usethis::use_data(raw_2021_release, overwrite = TRUE,internal = TRUE)
