# clean up raw data before importing into shiny app (app.R)

library(shiny)
library(tidyverse)
library(here)
library(shinythemes)
library(bslib)
library(lubridate)
library(tsibble)
library(sf)
library(tmap)
library(janitor)

#Load and wrangle spatial data
locations <- read_csv(here('data','site_locations.csv')) %>%
  drop_na()
locations_sf = st_as_sf(locations, coords = c("long", "lat"),
                 crs = 4326)
plot(locations_sf$geometry)

arizona_sf <- read_sf(here('data/tl_2020_04_county10/tl_2020_04_county10.shp')) %>%
  clean_names()

maricopa_sf <- arizona_sf %>%
  filter(name10 == 'Maricopa')

#quick visualization of both sf data
ggplot()+
  geom_sf(data = maricopa_sf) +
  geom_sf(data = locations_sf, size = 1, color = 'coral')

########################################################################################################################

#Load and wrangle plant/ arthropod data
plants_2008 <- read_csv(here("data","571_biomass_2008.csv"))
arthropods_2008 <- read_csv(here("data","571_arthropods_2008.csv"))

df_2008 <- plants_2008 %>% inner_join(arthropods_2008,
                                      by=c('plant_id','treatment_id','month', 'habitat_type','site_id','site_number','name'))
df_2008 <- df_2008 %>%
  mutate(year = 2008,
         year = ifelse(month == "December", "2007", year),
         date = format(lubridate::ymd(paste0(year, month,"01")), "%Y-%m-%d")) #%>%


colnames(df_2008)
unique(df_2008$habitat_type)
unique(df_2008$treatment_id)

df_2008 <- df_2008 %>%
  mutate(treatment_id, treatment_name = case_when(treatment_id=="R"~"low water + cage",
                                                  treatment_id=="O"~"low water + no cage",
                                                  treatment_id=="B"~"medium water + cage",
                                                  treatment_id=="G"~"medium water + no cage",
                                                  treatment_id=="Y"~"high water + cage",
                                                  treatment_id=="P"~"high water + no cage",
                                                  TRUE ~ treatment_id)) %>%
  select(date, month_number, month, year, site_id, name, habitat_type, plant_id, treatment_id, treatment_name, plant_dry_mass, genus, indiv_count) %>%
  mutate(treatment_name = factor(treatment_name, levels = c("low water + cage", "low water + no cage","medium water + cage","medium water + no cage","high water + cage","high water + no cage")))

unique(plants_2008$month)

write.csv(df_2008, "/Users/kristinart/Library/CloudStorage/GoogleDrive-kristinart@ucsb.edu/My Drive/Winter 2023/ESM 244/Assignments/Shiny App/244-shiny-app/data/df_final.csv")


