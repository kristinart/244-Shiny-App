#Attach packages
library(shiny)
library(tidyverse)
library(here)
library(shinythemes)
library(bslib)


#Load and wrangle data
plants_2007 <- read_csv(here("data","571_biomass_2007.csv"))
plants_2008 <- read_csv(here("data","571_biomass_2008.csv"))
arthropods_2007 <- read_csv(here("data","571_arthropods_2007.csv"))
arthropods_2008 <- read_csv(here("data","571_arthropods_2008.csv"))

#Note: for 2007, need to add column for treatment id based on the water and cage columns, using metadata from source.
#df_2007 <- plants_2007 %>% inner_join( arthropods_2007,
#                                       by=c('plant_id','treatment_id','month', 'habitat_type','site_id','site_number','name'))
#df_2007 <- df_2007 %>%
#  mutate(year = 2007)
df_2008 <- plants_2008 %>% inner_join( arthropods_2008,
                              by=c('plant_id','treatment_id','month', 'habitat_type','site_id','site_number','name'))
df_2008 <- df_2008 %>%
  mutate(year = 2008)

#Join 2007 and 2008 dataframes below
#for now, I'm going to rename df_2008 as our final df so we don't have to change how we call the data in our widgets later on:
df_combined <- df_2008 #%>%
#  mutate(treatment = case_when( ### Add a new column with treatment type spelled out so we can call that instead of just treatment_id. This code doesn't work, need to spend some time later to fix it or just manually rename the treatment ids in the widgets themselves
#    endsWith(treatment_id, 'R' ~ 'cage, low water'),
#    endsWith(treatment_id, 'O' ~ 'no cage, low water'),
#    endsWith(treatment_id, 'B' ~ 'cage, medium water'),
#    endsWith(treatment_id, 'G' ~ 'no cage, medium water'),
#    endsWith(treatment_id, 'Y' ~ 'cage, high water'),
#    endsWith(treatment_id, 'P' ~ 'no cage, high water')
#  ))
head(df_combined)

unique(df_combined$habitat_type)
unique(df_combined$treatment_id)



###############################################################################################################################################################################
# App building time!
# Create three panels/ tabs for the shiny app
# panel 1 will display background info including the experimental setup, habitat types, a map of the sites/ habitats in AZ
ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = NULL,
    bg = "white",
    fg = "black",
    primary = "purple",
    secondary = "turquoise",
    success = "green",
    info = "dodgerblue",
    warning = "yellow",
    danger = "red",
    base_font = font_google("Merriweather"),
    code_font = font_google("Asar"),
    heading_font = font_google("Gravitas One"),
    font_scale = 1
  ),
  navbarPage("Brittlebush Productivity and Arthropod Community Characteristics",
             tabPanel("panel_1",
                      titlePanel("Introduction and Background"),
                      img(src ="https://www.researchgate.net/profile/Christofer-Bang/publication/225081502/figure/fig2/AS:669081560707081@1536532883563/Map-of-the-Phoenix-metropolitan-area-with-approximate-location-of-the-two-weather.ppm"),
                      p("This study examined the species abundance, richness, and evenness of arthropods, and the plant productivity of brittlebush, in response to different habitats and treatment conditions. The purpose of the study was to better understand any potential impact of different habitat types and growing conditions on urban biodiversity."),
                      sidebarLayout(
                        sidebarPanel(
                          #selectInput(#radioButtons(inputId = "habitat_type",
                                                 #  label = "Choose habitat type",
                                                  # choices = unique(df_combined$habitat_type)
                                      ), #end of sidebar panel
                      #),#end of sidebar layout
                        mainPanel("output: summary map with sites of that habitat type highlighted/ selected and a short ~2-sentence summary blurb of what that habitat type refers to.")
                      )),
             tabPanel("panel_2",
                      titlePanel("Brittlebush Productivity Under Varying Conditions"),
                      p("Insert blurb on productivity of the plants under the various treatments"),
                      sidebarLayout(
                        sidebarPanel(
                        #selectInput(checkboxGroupInput(#inputId = "water",
                                                       #label = "Water treatment",
                                                       #choices = unique(df_combined$water)#,
                                                       #"cage", label = "Cage treatment",
                                                       #choices = unique(df_combined$cage)
                        ),
                        mainPanel("output: box and whisker plot of plant productivity under the chosen combination of treatment conditions")
                      )), #end of panel 2
             tabPanel("panel_3",
                      titlePanel("Arthropod Community Characteristics Under Varying Conditions"),
                      p("Insert blurb on arthropod community response to brittlebush productivity under varying conditions"),
                      sidebarLayout(
                        sidebarPanel(
                        #selectInput(#checkboxGroupInput(selectInput(inputId = "treatment_id",
                                                                  # label = "Select cluster treatment",
                                                                  # choices = unique(df_combined$treatment_id)
                          ),
                        mainPanel("output: ")
                      )#end of panel 3
             )#end of navbarPage
  ) #end of fluid page
)
server <-function(input, output){}
shinyApp(ui = ui, server = server)

###############################################################################################################################################################################

###############################################################################################################################################################################


# work on theme

theme1 <- bs_theme(
  version = 5,
  bootswatch = NULL,
  bg = "white",
  fg = "black",
  primary = "purple",
  secondary = "turquoise",
  success = "green",
  info = "dodgerblue",
  warning = "yellow",
  danger = "red",
  base_font = font_google("Merriweather"),
  code_font = font_google("Asar"),
  heading_font = font_google("Gravitas One"),
  font_scale = "1"
  )

#Helpful info
#fonts: https://fonts.google.com
#colors: https://r-charts.com/colors/
