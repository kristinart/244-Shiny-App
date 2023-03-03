# clean up raw data before importing into shiny app (app.R)

library(shiny)
library(tidyverse)
library(here)
library(shinythemes)
library(bslib)
library(lubridate)
library(tsibble)


#Load and wrangle data
#plants_2007 <- read_csv(here("data","571_biomass_2007.csv"))
plants_2008 <- read_csv(here("data","571_biomass_2008.csv"))
#arthropods_2007 <- read_csv(here("data","571_arthropods_2007.csv"))
arthropods_2008 <- read_csv(here("data","571_arthropods_2008.csv"))

#Note: for 2007, need to add column for treatment id based on the water and cage columns, using metadata from source.
# arthropods_2007 <- arthropods_2007 %>%
#   mutate(treatment_id=case_when(cage=="1" & water=="LOW" ~ "R",
#                                 cage=="0" & water=="LOW" ~ "O",
#                                 cage=="1" & water=="MEDIUM" ~ "B",
#                                 cage=="0" & water=="MEDIUM" ~ "G",
#                                 cage=="1" & water=="HIGH" ~ "Y",
#                                 cage=="0" & water=="HIGH" ~ "P",
#                                 TRUE ~'NA'))
#
# df_2007 <- plants_2007 %>% inner_join(arthropods_2007,
#                                       by=c('plant_id','treatment_id', 'habitat_type','site_id','site_number','name'))
# df_2007 <- df_2007 %>%
#   mutate(year = 2007) %>%
#   rename(super_family = 'superfamily') %>%
#   select(c(-'species',-'sum_biomass'))

df_2008 <- plants_2008 %>% inner_join(arthropods_2008,
                                      by=c('plant_id','treatment_id','month', 'habitat_type','site_id','site_number','name'))
df_2008 <- df_2008 %>%
  mutate(year = 2008,
         date = format(lubridate::mdy(paste0(month,"01", year)), "%m-%d-%Y")) #%>%

#colnames(df_2007)
# print(cols_ls)
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
  select(date, month_number, month, year, site_id, name, habitat_type, plant_id, treatment_id, treatment_name, plant_dry_mass, genus, indiv_count)
unique(plants_2008$month)
# common_col_names <- intersect(names(df_2007), names(df_2008))
# #Join 2007 and 2008 dataframes below
# df_combined <- rbind(df_2007, df_2008) %>%
#   drop_na(plant_dry_mass) %>%
#   mutate(date = format(lubridate::ymd(paste0(year,month,"01")), "%Y-%m"))
#
# head(df_combined)
#
# colnames(df_combined)
#df_combined <- df_combined %>%
#select(site_id, site_number, name, habitat_type, plant_id, treatment_id, plant_dry_mass, )
# unique(df_combined$habitat_type)
# unique(df_combined$treatment_id)

write.csv(df_2008, "/Users/kristinart/Library/CloudStorage/GoogleDrive-kristinart@ucsb.edu/My Drive/Winter 2023/ESM 244/Assignments/Shiny App/244-shiny-app/data/df_final.csv")


