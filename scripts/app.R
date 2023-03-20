
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
library(showtext)
font_add_google("Poppins", family = "special")

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
    base_font = font_google("Poppins"),
    code_font = font_google("Poppins"),
    heading_font = font_google("Poppins"),
    font_scale = 1
  ), ### end of theme
  navbarPage(title = "Brittlebush Productivity and Arthropod Community Characteristics",
                 tabPanel(title = "Background",
                 fluidPage(
                   titlePanel(h3("Introduction and Background", align = "center")),
                   br(),
                   fluidRow(
                     column(10, style = "background-color:#D4B2A7;",
                            br(),
                            p("An experimental field study was carried out by researchers at Arizona State University between 2007-2008 to
                       determine the effect of different environmental conditions on plant growth and associated arthropod communities.",
                       span("Encelia farinosa", style = "font-style:italic"),", a plant species commonly known as brittlebush, were grown
                       under varying levels of resource availability and predation to explore bottom-up and top-down effects on
                       brittlebush growth and associated arthropod communities. Brittlebush plants from 9 sites across the Arizona-Pheonix
                       metropolitan area spanning 3 different habitat types were used for this study. This app can be used to explore the
                       brittlebush and arthropod data collected for this study."),
                       br(),
                       strong("Habitat Types Overview"),
                       p(span("Urban ", style = "color:green"), "– sites selected in the city of Phoenix at human-altered landscapes (school yards/ campus). Includes landscapes near buildings and contains nonnative ornamental vegetation."),
                       p(span("Remnant ", style = "color:green"), "– sites selected in desert remnant habitat within or proximal to the city of Phoenix. Includes open landscapes and contains both native, perennial vegetation and non-native trees."),
                       p(span("Desert ", style="color:green"), "– sites selected in regional parks in the area around Phoenix. Includes open landscapes and primary contains native, perennial vegetation."),
                       br()
                     ), # end of columns 9
                     column(2,align="center", style = "background-color:#D4B2A7;",
                            br(),
                            img(src = "https://inaturalist-open-data.s3.amazonaws.com/photos/257913803/medium.jpeg", width = 175),
                            p("Image of a cactus wren on a cactus in Arizona.")
                     ) #end of columns 3
                   ),
                   fluidRow(
                     column(7, align="center",
                            br(),
                            checkboxGroupButtons(inputId = "map_habitat_type",
                                            label = "Select habitat type(s) below to view all study sites that are located in those habitat(s):",
                                            choices = c('Urban','Remnant','Desert'),
                                            selected = ('Desert'),
                                            size = 'normal',
                                            status = 'success',
                                            direction = 'horizontal'),
                       plotlyOutput(outputId = "map_plot"),
                       p("Figure 1: Interactive map of Maricopa County, Arizona. Triangular markers represent study sites and colors represent habitat types. Hover over a site to view its name and habitat type. Use the toolbar features to zoom, pan, compare hover points, and more.")
                               ), #end of columns 12
                     column(5, align = "center",
                            br(),
                            selectInput(inputId = "habitat_type_photo",
                                        label = "Select habitat type to view image of a representative study site:",
                                        choices = c('Urban - Arizona State University Campus' = 'Urban',
                                                    'Remnant - South Mountain' = 'Remnant',
                                                    'Desert - White Tank Mountain Regional Park' = 'Desert'),
                                        selected = 'Desert'),
                            imageOutput("image_habitat"),
                            p("Image: Photos from sample sites of each habitat type. Use the drop-down menu to select a habitat type image to view."),
                            br()
                     )
                     ), #end of fluid row
                   fluidRow(
                     column(12, style = "background-color:#D4B2A7;",
                            br(),
                            strong("Data Citations:"),
                            p("Bang, C. 2013. Control of arthropod abundance, richness, and composition in the central Arizona-Phoenix metropolitan area ver 6. Environmental Data Initiative. https://doi.org/10.6073/pasta/22e305a8950091fe4d71ebec6142ecea (Accessed 2023-02-02)"),
                            p("TIGER/Line Shapefile, 2019. state, Arizona, Current Block Group State-based. 2021. catalog.data.gov."),
                            br()
                     )
                   ),#end of fluidrow
                   br()
                   ) #end of fluid page
                 ), #end of tab 1

             tabPanel("Brittlebush",
                 fluidPage(titlePanel(h3("Brittlebush Productivity Under Varying Conditions", align = "center")),
                           br(),
                           fluidRow(
                             column(8, style = "background-color:#D4B2A7;",
                                    br(),
                                    p("This study focused on brittlebush ", span("(Encelia farinosa)", style = "font-style:italic"),
                                      ", a native perennial shrub, because of its ability to support many different species of arthropods
                            and its role as a foraging site for birds that prey on arthropods. Brittlebush plants involved in the study were grown under different cluster treatments of water availability
                            (low, medium, or high) and cage presence (with or without). The varying water availability was chosen to
                            determine the effect of a bottom-up condition and the cage presence was chosen to determine the effect of top-down
                            predation by birds that could prey on arthropods associated with brittlebush plants."),
                            p("The plants grown with a “no cage” treatment tended to have a wider range of plant dry masses.
                            These plants were more likely to be more productive than their “cage” treatment counterparts at the same watering level.
                            This is likely because “no cage” allowed birds to prey on arthropods, limiting the arthropod populations that
                            reduce plant biomass. The “cage” plants had greater arthropod abundance, which means there were more arthropods to eat the plants."),
                            p("While the study found that water had a positive effect on plant growth, this effect is complicated by interactions with other
                            factors like plants' site habitat type and the presence or absence of a cage. For example, the median plant dry mass appears to
                            be slightly higher for plants in the “high water + no cage” cluster treatment group compared to the “low water + no cage” and “medium water + no cage” plants."),
                            p("Brittlebush productivity also varied across the different habitat types. For more information on habitat types, see the ", span("Background", style="font-weight:bold" )," tab."),
                            br(),
                                    ), #end of columns 9
                             column (4, align = 'center', style = "background-color:#D4B2A7;",
                                     br(),
                                     br(),
                                     br(),
                                     img(src = "https://inaturalist-open-data.s3.amazonaws.com/photos/76846016/small.jpg", width = 350),
                                     p("Image of a brittlebush plant.")
                                     ) # end of columns 3
                           ),
                           fluidRow(
                             column(1),
                             column(10, align = 'center',
                                    br(),
                                    p("Select the cluster treatment(s) brittlebush were grown under to see the resulting plant dry mass(es), a measure of plant productivity.", "\n",
                              "For a detailed data table filtered by selected cluster treatment, toggle “View Data Table” to “ON.”"),
                              br()
                             ),
                             column(1)
                           ),
                       sidebarLayout(
                            sidebarPanel(width = 3,
                              br(),
                              br(),
                              checkboxGroupInput(inputId = "treatment_name_plant",
                                           label = "Select cluster treatment(s) to view plant dry mass(es)",
                                           choices = c("low water + cage", "low water + no cage","medium water + cage","medium water + no cage","high water + cage","high water + no cage"),
                                           selected = c(df_final$treatment_name[1], 'low water + no cage')),
                              br(),
                              br(),
                              br(),
                              switchInput(
                                inputId = "switch",
                                label = "View Data Table",
                                labelWidth = "80px",
                                onLabel = "YES",
                                offLabel = "NO",
                              ),
                              br()
                                    ), # end sidebar panel
                          mainPanel(plotOutput(outputId = "plant_treatment_plot"),
                                    p("Figure 2: Violin plot of plant productivity under the chosen cluster treatments. The width of each curve corresponds to the density or frequency of data points observed in that region.")
                                    )
                 ), #end of sidebar layout
                 fluidRow(
                   column(12, align = 'center',
                          br(),
                          br(),
                          p("Table 1: Interactive data table of brittlebush included under the selected cluster treatment(s).
                            Specify the number of entries to show, use the searchbox to find key terms, and sort the table by columns of interest."),
                          dataTableOutput("table")
                          ) #end of columns 12
                 ) #end of fluidrow
                 ) #end of fluidpage
                 ), #end of tab 2

             tabPanel(title = "Arthropods",
                          fluidPage(
                            titlePanel(h3("Arthropod Abundance on Brittlebush Grown Under Varying Conditions", align = "center")),
                            br(),
                            fluidRow(
                              column(5,style = "background-color:#D4B2A7;",
                                     br(),
                                     p("Arthropod species associated with each brittlebush plant included in the study were identified and counted.
                                       The total count of all arthropods observed are displayed in Figures 3 and 4. The amount of arthropods observed
                                       on a brittlebush is affected by the brittlebush's productivity and the ability for birds to predate on arthropods;
                                       the presence of a cage surrounding a brittlebush was effective at preventing predation by birds."),
                                     p("Total arthropod counts increased over the study period and peaked in late spring when brittlebush biomass also peaked.
                                       The impact of treatment types on total arthropod count varied greatly by habitat type."),
                                     br(),
                                     img(src = "https://inaturalist-open-data.s3.amazonaws.com/photos/227161942/medium.jpeg", width = 575),
                                     p("Image of arthropods on a brittlebush plant."),
                                     br()
                              ),
                              column(7,style = "background-color:#D4B2A7;",
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     plotOutput(outputId = "date_plot"),
                                     p("Figure 3: Graph of total monthly plant biomass and total monthly arthropods over the study period. The columns
                                       represent total plant biomass across all sites and the black line graph represents total arthropod counts per plant
                                       across all sites. Month number correlates to the experimental observation month where Month 1 is December 2007 and
                                       Month 6 is May 2008.")
                                     )
                            ),
                            br(),
                            fluidRow(
                              column(12, align = 'center',
                                     p("Select the plant cluster treatment to view the monthly total arthropod counts associated with brittlebush plants grown
                                       under that treatment:"),
                                     br())
                            ),
                            sidebarLayout(
                              sidebarPanel(width = 3,
                                br(),
                                br(),
                                br(),
                                radioButtons("treatment_name",
                                            "Select plant cluster treatment to view associated arthropod abundance",
                                            choices = c("low water + cage", "low water + no cage","medium water + cage","medium water + no cage","high water + cage","high water + no cage"),
                                            selected = df_final$treatment_name[1]),
                                br(),
                                br(),
                                br(),
                                br(),
                                br()
                                ), #end of sidebar panel
                              mainPanel(
                                plotOutput(outputId = "arth_treatment_plot"),
                                p("Figure 4: Line graph of total monthly arthropod counts observed on brittlebush plants grown under selected cluster treatment.
                                  Line colors represent habitat types.")
                              ) # end of main panel
                              ) #end of sidebar layout
                            ) #end of fluidpage
                 ) #end of tab 3
                 )#end of navbarPage
)

#define app's server
server <-function(input, output, session){

    # widget 0: photo
   habitat_type_photo <- reactive({
   if(input$habitat_type_photo == TRUE){
     image(src = "image_habitat")
   }
   else{
   }
   }
   )

  #   ### Define habitat images
   output$image_habitat <- renderImage({
     ### When input$n is 'Urban', filename is ./photos/image_Urban.jpg
    filename <- here('./photos',paste('image_', input$habitat_type_photo, '.jpg', sep=''))
    list(src = filename,
         alt = paste("Habitat type: ", input$habitat_type_photo), width = 500, height = 350)

  }, deleteFile = FALSE)

    #widget 1: map
  map_habitat_select <- reactive({
    locations_sf %>%
      filter(habitat_type %in% input$map_habitat_type)
  })

  col <- c('Urban' = "#5B1A18", 'Remnant' = "#FD6467", 'Desert' = "#A2A475")

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
        legend.box.background = element_rect(fill='transparent'), #transparent legend panel
        legend.title=element_text(size=14, family = "special"),
        legend.text=element_text(size=12, family = "special"),
        axis.text.x=element_text( size = 12, family = "special"), #angle=45,hjust=1,
        axis.text.y=element_text(size = 12, family = "special"),
        axis.title=element_text(size=14, family = "special"),
        plot.title = element_text(size = 20, family = "special", hjust = 0.5)
      )

    ggplotly(map, tooltip = "text")
  })

  # widget2: plant means by treatment type
  plant_treatment_select <- reactive({
    df_final %>%
      filter(treatment_name == input$treatment_name_plant) %>%
      drop_na(plant_dry_mass)
  })

  col2 <- c("low water + cage" = "#F1BB7B", "low water + no cage" = "#FD6467","medium water + cage"="#5B1A18","medium water + no cage"="#D67236","high water + cage"="#A2A475","high water + no cage"="#FAEFD1")

  output$plant_treatment_plot <- renderPlot({
    ggplot(data = plant_treatment_select(), aes(x = treatment_name,
                                                y = plant_dry_mass,
                                                group = treatment_name,
                                                fill = treatment_name)) +
      geom_violin(trim=FALSE)+
      #geom_boxplot()+
      scale_fill_manual(values = col2)+
      labs(x = "Cluster Treatment",
           y = "Plant Dry Mass (g)",
           fill = 'Treatment Type',
           title = "Average Brittlebush Dry Mass (g) Under Cluster Treatments") +
      theme_minimal()+
      theme(axis.text.x=element_text( size = 12, family = "special"), #angle=45,hjust=1,
            axis.text.y=element_text(size = 12, family = "special"),
            axis.title=element_text(size=16, family = "special"),
            plot.title = element_text(size = 20, hjust = 0.5, family = "special"),
            legend.title=element_text(size=16, family = "special"),
            legend.text=element_text(size=14, family = "special"))

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
       labs(x = 'Date', y = 'Total Count', colour = 'Habitat Type', title = (paste0('Total arthropod count by month on brittlebush plants treated with ',treatment_title())))+
       scale_color_manual(values= wes_palette("GrandBudapest1", n = 3))+
       theme_minimal()+
       theme(axis.text.x=element_text( size = 12, family = "special"), #angle=45,hjust=1,
             axis.text.y=element_text(size = 12, family = "special"),
             axis.title=element_text(size=16, family = "special"),
             plot.title = element_text(size = 20, hjust = 0.5, family = "special"),
             legend.title=element_text(size=16, family = "special"),
             legend.text=element_text(size=14, family = "special"))

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
       geom_col(aes(x=month_number, y=plant_dry_mass, fill = factor(month_number)))+ #, alpha = 0.6
       geom_line(aes(x=month_number, y=100*indiv_count),color="black",size=2)+
       scale_fill_manual(values= c("#F1BB7B", "#FD6467", "#5B1A18", "#D67236","#A2A475","#FAEFD1"))+
       coord_cartesian(xlim=input$date_slider)+
       labs(x="Month Number",y="Plant Biomass (g)", fill = "Month Number", title = "Total monthly brittlebush biomass and arthropod counts per plant")+
       scale_y_continuous(sec.axis=sec_axis(~.*0.01,name="Arthropods per plant"))+
       theme_minimal()+
       theme(legend.position = "none",
             axis.text.x=element_text( size = 12, family = "special"), #angle=45,hjust=1,
             axis.text.y=element_text(size = 12, family = "special"),
             axis.title=element_text(size=16, family = "special"),
             plot.title = element_text(size = 20, family = "special", hjust = 0.5),
             panel.grid.major = element_blank(), #remove major gridlines
             panel.grid.minor = element_blank() #remove minor gridlines
             )

   },bg = 'transparent')

}

#combine ui and server
shinyApp(ui = ui, server = server)

