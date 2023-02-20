#Attach packages
library(shiny)
library(tidyverse)
library(here)
library(shinythemes)


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
df_combined <- df_2008 %>%
  mutate(treatment = case_when( ### Add a new column with treatment type spelled out so we can call that instead of just treatment_id. This code doesn't work, need to spend some time later to fix it or just manually rename the treatment ids in the widgets themselves
    endsWith(treatment_id, 'R' ~ 'cage, low water'),
    endsWith(treatment_id, 'O' ~ 'no cage, low water'),
    endsWith(treatment_id, 'B' ~ 'cage, medium water'),
    endsWith(treatment_id, 'G' ~ 'no cage, medium water'),
    endsWith(treatment_id, 'Y' ~ 'cage, high water'),
    endsWith(treatment_id, 'P' ~ 'no cage, high water')
  ))
head(df_combined)

unique(df_combined$habitat_type)
unique(df_combined$treatment_id)



###############################################################################################################################################################################
# App building time!
# Create three panels/ tabs for the shiny app
# panel 1 will display background info including the experimental setup, habitat types, a map of the sites/ habitats in AZ
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  navbarPage("Brittlebush Productivity and Arthropod Community Characteristics",
             tabPanel("panel_1",
                      titlePanel("Introduction and Background"),
                      img(src ="https://www.researchgate.net/profile/Christofer-Bang/publication/225081502/figure/fig2/AS:669081560707081@1536532883563/Map-of-the-Phoenix-metropolitan-area-with-approximate-location-of-the-two-weather.ppm"),
                      p("Let's insert a summary blurb about the experiment here"),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(radioButtons(inputId = 'habitat_type',
                                                              label = "Choose habitat type",
                                                              choices = c("Urban"= Urban,"Desert"= Desert,"Remnant"= Remnant))),
                        ), #end of sidebar panel 1
                        mainPanel("output: summary map with sites of that habitat type highlighted/ selected and a short ~2-sentence summary blurb of what that habitat type refers to.")
                      )),#end of panel 1
             tabPanel("panel_2",
                      titlePanel("Brittlebush Productivity Under Varying Conditions"),
                      p("Insert blurb on productivity of the plants under the various treatments"),
                      sidebarPanel(
                        selectInput(checkboxGroupInput("water", label = "Water treatment",
                                                       choices = c("low water" = LOW, "medium water" = MEDIUM, "high water" = HIGH),
                                                       "cage", label = "Cage treatment",
                                                       choices = list("cage" = 1, "no cage" = 0)))),
                      mainPanel("output: box and whisket plot of plant productivity under the chosen combination of treatment conditions")
                      ), #end of panel 2
             tabPanel("panel_3",
                      titlePanel("Arthropod Community Characteristics Under Varying Conditions"),
                      p("Insert blurb on arthropod community response to brittlebush productivity under varying conditions"),
                      sidebarPanel(
                        selectInput(checkboxGroupInput(selectInput(inputId = 'treatment_id',
                                                                   label = "Select cluster treatment",
                                                                   choices = c("R","O","B","G","Y","P"))))),
                      mainPanel("output: ")
                      )#end of panel 3
             )#end of navbarPage
  ) #end of fluid page

server <-function(input, output){}
shinyApp(ui = ui, server = server)


server <- function(input, output) {
  # You can access the values of the widget (as a vector) with input$checkGroup, e.g.
  output$value <- renderPrint({ input$checkGroup })}
shinyApp(ui = ui, server = server)

###############################################################################################################################################################################

###############################################################################################################################################################################
