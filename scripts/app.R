
# This is a Shiny web application. You can run the application by clicking the 'Run App' button above.

library(shiny)
library(tidyverse)
library(here)
library(shinythemes)
library(bslib)
library(cowplot)
library(wesanderson)
library(plotly)
library(sf)
library(janitor)
library(shinyWidgets)
library(DT)

#Load plant and arthropod data
df_final <- read_csv(here('data','df_final.csv'))

### Load and wrangle spatial data
locations <- read_csv(here('data','site_locations.csv')) %>%
  drop_na()

locations_sf <- st_as_sf(locations, coords = c("long", "lat"),
                        crs = 4326) %>%
  mutate(text = paste0("Site Name: ", name, "\n", "Habitat Type: ", habitat_type))

arizona_sf <- read_sf(here('data/tl_2020_04_county10/tl_2020_04_county10.shp')) %>%
  clean_names()

maricopa_sf <- arizona_sf %>%
  filter(name10 == 'Maricopa')


#define user interface
ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = NULL,
    bg = "antiquewhite",
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
  ), ### end of theme
  navbarPage(title = "Brittlebush Productivity and Arthropod Community Characteristics",
                 tabPanel(title = "Background",
                 fluidPage(
                   titlePanel("Introduction and Background"),
                 #  img(src ="https://www.researchgate.net/profile/Christofer-Bang/publication/225081502/figure/fig2/AS:669081560707081@1536532883563/Map-of-the-Phoenix-metropolitan-area-with-approximate-location-of-the-two-weather.ppm"),
                     p("Understanding the impacts of bottom-up and top-down management strategies on
                       species is important for effectively managing urban ecosystems. This study examined the impact of
                       changes in resource availability (a bottom-up strategy) and predation by birds (a top-down strategy)
                       on the abundance, richness, and composition of arthropod species in the Phoenix metropolitan area in Arizona."),
                     strong("Habitat Type Overview"),
                     p("Nine sites were selected for this study."),
                     p(span("Urban ", style = "color:purple"), "– sites selected in the city of Phoenix at human-altered landscapes (school yards/ campus). Landscapes near buildings and nonnative ornamental vegetation."),
                     p(span("Remnant ", style = "color:purple"), "– sites selected in desert remnant sites within or proximal to the city of Phoenix. Open landscapes. Similar native, perennial vegetation to desert sites, but near nonnative trees."),
                     p(span("Desert ", style="color:purple"), "– sites selected in regional parks in the area around Phoenix. Open landscapes. Primarily native, perennial vegetation."),
                   #img(src = "https://inaturalist-open-data.s3.amazonaws.com/photos/76846016/small.jpg",
                       #here("photos", "brittlebush_iNaturalist_76846174_SimonTonge.jpg",
                             #     align = 'center',
                            #      height = '100px',
                            #      width = '100px'
                            #), # end fluidPage
                    img(src = "brittlebush_iNaturalist_76846174_SimonTonge.jpg"),
                   sidebarLayout(
                     sidebarPanel(
                       checkboxGroupButtons(inputId = "map_habitat_type",
                                          label = "Select Habitat Type",
                                          choices = c('Urban','Remnant','Desert'),
                                          selected = ('Urban'),
                                          size = 'sm',
                                          direction = 'vertical')
                     ), #end of sidebar panel
                     mainPanel(#p("Output: habitat bar plot"),
                               plotlyOutput(outputId = "map_plot")
                               ) #end of main panel
                 ) #end of sidebar layout
                 ) #end of fluid page
                 ), #end of tab 1

                 tabPanel("Brittlebush",
                 fluidPage(titlePanel("Brittlebush Productivity Under Varying Conditions"),
                          p("Arthropods are dependent on plants for resources. This study focused on brittlebush ",
                            span("(Encelia farinosa)", style = "font-style:italic"),
                            "a native perennial shrub, because of its ability to support many different species of arthropods,
                            and its function as a foraging site for birds that prey on arthropods."),
                          p("Plants were examined in each of the different habitats under different watering conditions (low, medium, or high) and with or without cages. The presence or absence of a cage impacts whether or not birds could prey on arthropods on that plant. This impacted arthropod abundance, which in turn impacted plant productivity."),
                          p("Select the treatment given to a cluster of plants to see the resulting plant dry mass, a measure of plant productivity."),
                          p("The plant clusters with a “no cage” treatment tended to have a wider range of plant dry masses. These plants were more likely to be more productive than their “cage” treatment counterparts at the same watering level. This is likely because “no cage” allowed birds to prey on arthropods, limiting the arthropod population that
                            eats the plants. The “cage” plants had greater arthropod abundance, which means there were more arthropods to eat the plants."),
                          p("While the study found that water had a positive effect on plant growth, this effect is complicated by interactions with other factors like plant’s site habitat type and the presence or absence of a cage. For example, the median plant dry mass appears to be slightly higher for plants in the high water + no cage cluster
                            treatment group than it is for low or medium water, no-cage plants."),
                          p("Brittlebush productivity also varied across the different habitat types. For more information on habitat types, see the ", span("Background", style="font-weight:bold" )," tab."),
                          p("For a detailed data table filtered by selected cluster treatment, click on the “View Data” toggle."),

                       sidebarLayout(
                            sidebarPanel(
                              checkboxGroupInput(inputId = "treatment_name_plant",
                                           label = "Select Cluster Treatment",
                                           choices = c("low water + cage", "low water + no cage","medium water + cage","medium water + no cage","high water + cage","high water + no cage"),
                                           selected = c(df_final$treatment_name[1], 'medium water + cage')),
                              switchInput(
                                inputId = "switch",
                                label = "View Data",
                                labelWidth = "80px",
                                onLabel = "YES",
                                offLabel = "NO",
                              )
                                    ), # end sidebar panel
                          mainPanel(#p("output: box and whisker plot of plant productivity under the chosen combination of treatment conditions"),
                                    plotOutput(outputId = "plant_treatment_plot"),
                                    dataTableOutput("table")
                                    )
                 ) #end of sidebar layout
                 ) #end of fluidpage
                 ), #end of tab 2

                 tabPanel(title = "Arthropods",
                          fluidPage(
                            titlePanel("Arthropod Abundance on Brittlebush Grown Under Varying Conditions"),
                            # p("Insert blurb on arthropod community response to brittlebush productivity under varying conditions.
                            #   Planning to clean up and pretty both plots. For bottom plot, hoping
                            #   to get a second y-axis to display plant dry mass too; since they are of such different magnitudes,
                            #   it doesn't look good to have them plotted on the same axis. Also hoping to get the slider widget and
                            #   x-axis of the plot to have month names (Jan Feb Mar, etc) as the tick labels. Lastly, open to displaying
                            #   something else on this tab...we originally talked about species richness but idrk if I want to try to
                            #   calculate that with the unclear labels in this dataset..."),

                            p("Arthropods are invertebrate animals such as insects. The study measured the total count of arthropods
                              observed each month in three habitat types in response to changes in watering level and cage absence or
                              presence. Plant growth is water-dependent. The presence or absence of a cage impacts whether or not birds
                              could prey on arthropods on that plant. Availability of plant biomass and whether or not they have protection
                              from predators both impact arthropod abundance."),
                            p("Select the treatment type given to a cluster of plants to see the resulting total arthropod count per month
                              by habitat type in the line graph."),
                            p("The bar graph provides additional information: the bars show the plant biomass in each month of the study across
                              all habitat types. The black line shows the total count of arthropods per plant. The month number correlates to the
                              month of observation; month 1 is December 2007, and month 6 is May 2008."),
                            p("Arthropod counts and plat biomass increased to their peak in late spring. The impact of treatment types on total
                              arthropod count varied greatly by habitat type."),

                            sidebarLayout(
                              sidebarPanel(
                                radioButtons("treatment_name",
                                            "Select Cluster Treatment",
                                            choices = c("low water + cage", "low water + no cage","medium water + cage","medium water + no cage","high water + cage","high water + no cage"),
                                            selected = df_final$treatment_name[1]),
                                ), #end of sidebar panel
                              mainPanel(fluidRow(
                                splitLayout(cellWidths = c("50%", "50%"),
                                plotOutput(outputId = "arth_treatment_plot"),
                                plotOutput(outputId = "date_plot")))
                              ) # end of main panel
                              ) #end of sidebar layout
                            ) #end of fluidpage
                 ) #end of tab 3
                 )#end of navbarPage
)

#define app's server
server <-function(input, output, session){
  #widget 1: map
  map_habitat_select <- reactive({
    locations_sf %>%
      filter(habitat_type %in% input$map_habitat_type)
  })

  col <- c('Urban' = "#FD6467", 'Remnant' = "#5B1A18", 'Desert' = "#A2A475")

  output$map_plot <- renderPlotly({
    map <- ggplot()+
      geom_sf(data = maricopa_sf, color = 'black', fill = "#F1BB7B") +
      geom_sf(data = map_habitat_select(), size = 2, shape = 17, aes(text = text, color = habitat_type))+
      scale_color_manual(values= col)+
      labs(color = "Habitat Type")+
      theme_minimal()+
      theme(
        panel.background = element_rect(fill='transparent'), #transparent panel bg
        plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
        panel.grid.major = element_blank(), #remove major gridlines
        panel.grid.minor = element_blank(), #remove minor gridlines
        legend.background = element_rect(fill='transparent'), #transparent legend bg
        legend.box.background = element_rect(fill='transparent') #transparent legend panel
      )

    ggplotly(map, tooltip = "text")
  })

  # widget2: plant means by treatment type
  plant_treatment_select <- reactive({
    df_final %>%
      filter(treatment_name == input$treatment_name_plant) %>%
      drop_na(plant_dry_mass)
  })

  output$plant_treatment_plot <- renderPlot({
    ggplot(data = plant_treatment_select(), aes(x = treatment_name,
                                                y = plant_dry_mass,
                                                group = treatment_name,
                                                fill = treatment_name)) +
      geom_violin(trim=FALSE)+
      scale_fill_manual(values= c("#F1BB7B", "#FD6467", "#5B1A18", "#D67236","#A2A475","#FAEFD1"))+
      labs(x = "Cluster Treatment",
           y = "Plant Dry Mass ()",
           fill = 'Treatment Type',
           title = "Brittlebush Growth Resulting from Treatments") +
      theme_minimal()

  },bg = 'transparent')

  #widget 3: data table
  table <- reactive({
    if (input$switch == 'TRUE'){
      df_final %>%
        select(date, plant_id, habitat_type, site_id, name, treatment_name, plant_dry_mass) %>%
        drop_na(plant_dry_mass) %>%
        filter(treatment_name == input$treatment_name_plant)
    }
    else{
      df_final %>%
        select(date, plant_id, habitat_type, site_id, name, treatment_name, plant_dry_mass) %>%
        drop_na(plant_dry_mass) %>%
        slice(0)
    }
    })

  output$table <- renderDataTable({
    table()
  })

   # widget 4: arthropods by treatment type
   arth_treatment_select <- reactive({
     df_final %>%
       filter(treatment_name == input$treatment_name) %>%
       drop_na(indiv_count) %>%
       group_by(date,  habitat_type) %>% #site_id,
       summarise(sum(indiv_count)) %>%
       rename('total_arth' = 3) %>%
       arrange(date)
     })

   treatment_title <- reactive({
     df_final %>%
       filter(treatment_name == input$treatment_name) %>%
       select(treatment_name) %>%
       unique()
   })

   output$arth_treatment_plot <- renderPlot({
     ggplot(data = arth_treatment_select(), aes(x = date, y = total_arth, colour = habitat_type)) +
       geom_line(aes(colour = habitat_type, group = habitat_type), size = 3) +
       labs(x = 'Date', y = 'Total Count', colour = 'Habitat Type', title = str_wrap(paste0('Total arthropod count by month on brittlebush plants treated with ',treatment_title())))+
       scale_color_manual(values= wes_palette("GrandBudapest1", n = 3))+
       theme_minimal()+
       theme(axis.text.x=element_text(angle=45,hjust=1, size = 10))

   },bg = 'transparent')

   #additional figure, non-reactive (no widget)
   date_select <- #reactive({
     df_final %>%
       select(month,month_number, plant_dry_mass, indiv_count) %>%
       group_by(month, month_number) %>%
       summarise(across(c(indiv_count, plant_dry_mass), ~ mean(.x, na.rm = TRUE))) %>%
       arrange(month_number)
   #})

   output$date_plot <- renderPlot({
     #date_select() %>%
     date_select %>%
       ggplot()+
       geom_col(aes(x=month_number, y=plant_dry_mass, fill = factor(month_number)), alpha = 0.6)+
       geom_line(aes(x=month_number, y=100*indiv_count),color="black",size=2)+
       scale_fill_manual(values= c("#F1BB7B", "#FD6467", "#5B1A18", "#D67236","#A2A475","#FAEFD1"))+
       coord_cartesian(xlim=input$date_slider)+
       labs(x="Month Number",y="Plant Biomass ()", fill = "Month Number")+
       scale_y_continuous(sec.axis=sec_axis(~.*0.01,name="Arthropods per plant"))+
       theme_minimal()+
       theme(legend.position = "none")

   },bg = 'transparent')

}

#combine ui and server
shinyApp(ui = ui, server = server)

