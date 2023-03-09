
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

df_final <- read_csv(here('data','df_final.csv'))
#Load and wrangle spatial data
locations <- read_csv(here('data','site_locations.csv')) %>%
  drop_na()

locations_sf = st_as_sf(locations, coords = c("long", "lat"),
                        crs = 4326) %>%
  mutate(text = paste0("Site Name: ", name, "\n", "Habitat Type: ", habitat_type))

arizona_sf <- read_sf(here('data/tl_2020_04_county10/tl_2020_04_county10.shp')) %>%
  clean_names()

maricopa_sf <- arizona_sf %>%
  filter(name10 == 'Maricopa')


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
                     p("This study examined the species abundance, richness, and evenness of arthropods, and the plant productivity of brittlebush, in response to different habitats and treatment conditions.
                     The purpose of the study was to better understand any potential impact of different habitat types and growing conditions on urban biodiversity."),
                   #img(src = "https://inaturalist-open-data.s3.amazonaws.com/photos/76846016/small.jpg",
                       #here("photos", "brittlebush_iNaturalist_76846174_SimonTonge.jpg",
                             #     align = 'center',
                            #      height = '100px',
                            #      width = '100px'
                            #), # end fluidPage
                   sidebarLayout(
                     sidebarPanel(
                       # (checkboxGroupInput(inputId = "habitat_type",
                       #               label = "Choose habitat type",
                       #               choices = c("Desert", "Remnant", "Urban"))
                       # ) #end of checkboxGroup
                     ), #end of sidebar panel
                     mainPanel(#p("Output: habitat bar plot"),
                               #plotOutput(outputId = "habitat_plot"),
                               plotlyOutput(outputId = "map_plot")
                               ) #end of main panel
                 ) #end of sidebar layout
                 ) #end of fluid page
                 ), #end of tab 1

                 tabPanel("Brittlebush",
                 fluidPage(titlePanel("Brittlebush Productivity Under Varying Conditions"),
                          p("Insert blurb on productivity of the plants under the various treatments.
                            Notes: this widget is based on treatment_name."),
                          sidebarLayout(
                            sidebarPanel(
                              checkboxGroupInput(inputId = "treatment_name_plant",
                                           label = "Select Cluster Treatment",
                                           choices = unique(df_final$treatment_name),
                                           selected = c(df_final$treatment_name[1], 'medium water + cage'))
                                                 # choices = c("Low water (cage)" = "R",
                                                 #             "Low water (no cage)" = "O",
                                                 #             "Medium water (cage)" = "B",
                                                 #             "Medium water (no cage)" = "G",
                                                 #             "High water (cage)" = "Y",
                                                 #             "High water (no cage)" = "P"))
                                           #unique(df_final$treatment_name))
                                                 #choices = c("LOW" = "LOW", "MEDIUM" = "MEDIUM", "HIGH" = "HIGH"))#,
                                    ), # end sidebar panel
                          mainPanel(#p("output: box and whisker plot of plant productivity under the chosen combination of treatment conditions"),
                                    plotOutput(outputId = "plant_treatment_plot"))
                 ) #end of sidebar layout
                 ) #end of fluidpage
                 ), #end of tab 2

                 tabPanel(title = "Arthropods",
                          fluidPage(
                            titlePanel("Arthropod Abundance on Brittlebush Grown Under Varying Conditions"),
                            p("Insert blurb on arthropod community response to brittlebush productivity under varying conditions.
                              Planning to clean up and pretty both plots. For bottom plot, hoping
                              to get a second y-axis to display plant dry mass too; since they are of such different magnitudes,
                              it doesn't look good to have them plotted on the same axis. Also hoping to get the slider widget and
                              x-axis of the plot to have month names (Jan Feb Mar, etc) as the tick labels. Lastly, open to displaying
                              something else on this tab...we originally talked about species richness but idrk if I want to try to
                              calculate that with the unclear labels in this dataset..."),
                            sidebarLayout(
                              sidebarPanel(
                                radioButtons("treatment_name",
                                            "Select Cluster Treatment",
                                            choices = unique(df_final$treatment_name),
                                            selected = df_final$treatment_name[1]),
                                sliderInput(inputId = "date_slider",
                                            label = "Select Month Range",
                                            min = 1,
                                            max = 6,
                                            value = c(1,6),
                                            step = 1)
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

server <-function(input, output, session){


  output$map_plot <- renderPlotly({
    ggplot()+
      geom_sf(data = maricopa_sf, color = 'black', fill = "#F1BB7B") +
      geom_sf(data = locations_sf, size = 2, shape = 17, aes(text = text, color = habitat_type))+
      scale_color_manual(values= c("#FD6467", "#5B1A18", "#A2A475"))+
      labs(color = "Habitat Type")+
      theme_minimal()

  })

  # #widget_habitat_type data
  # habitat_select <- reactive({
  #   df_final %>%
  #     filter(habitat_type == input$habitat_type)
  # })
  #
  # #widget_habitat_type plot
  # output$habitat_plot <- renderPlot({
  #   ggplot(data = habitat_select(),
  #          aes(x = habitat_type,
  #              y = plant_dry_mass) +
  #            geom_point()
  #   )
  #
  # })

  ### widget1_habitat_type
  # habitat_select <- reactive({
  #      df_final %>%
  #        filter(habitat_type == input$habitat_type)
  # })
  #
  # output$habitat_image <- renderPlot({
  #   ggdraw () +
  #     draw_image("https://images.unsplash.com/photo-1470164971321-eb5ac2c35f2e?ixlib=rb-4.0.3&ixid=MnwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8&auto=format&fit=crop&w=1174&q=80")
  # })

  #widget1_habitat_type data
  habitat_select <- reactive({
    df_final %>%
      filter(habitat_type %in% input$habitat_type) %>%
      group_by(name, habitat_type) %>%
      summarize(num_of_obs = n())
  })
  ############Check this because I think I might have the wrong data count

  # widget1_habitat_type plot
  output$habitat_plot <- renderPlot({
    ggplot(data = habitat_select()) +
      geom_col(aes(x = name,
                   y = num_of_obs,
                   color = habitat_type
      )) +
      scale_color_manual(values = c("black", "coral", "turquoise")) +
      labs(x = "site name",
           y = "number of observations",
           title = "Number of Observations by Site Name and Habitat Type") +
      theme_minimal()
  },bg = 'transparent')
########change these to fill colors, and match theme



  # widget2_plant_treatment_type data
  plant_treatment_select <- reactive({
    df_final %>%
      filter(treatment_name == input$treatment_name_plant) %>%
      drop_na(plant_dry_mass) #%>%
      #group_by(treatment_name) %>%
      #summarize(mean(plant_dry_mass)) #%>%
      #rename('avg_plant_mass' = 3)
  })

  #widget2_plant_treatment_type plot
  output$plant_treatment_plot <- renderPlot({
    ggplot(data = plant_treatment_select(), aes(x = treatment_name,
                                                y = plant_dry_mass,
                                                group = treatment_name,
                                                fill = treatment_name)) +
      #geom_boxplot(outlier.shape = NA) +
      #geom_jitter()+
      geom_violin(trim=FALSE)+
      scale_fill_manual(values= c("#F1BB7B", "#FD6467", "#5B1A18", "#D67236","#A2A475","#FAEFD1"))+
      labs(x = "Cluster Treatment",
           y = "Plant Dry Mass ()",
           fill = 'Treatment Type',
           title = "Brittlebush Growth Resulting from Treatments") +
      theme_minimal()

  },bg = 'transparent')

   # widget3_arthropod_treatment_id data
   arth_treatment_select <- reactive({
     df_final %>%
       filter(treatment_name == input$treatment_name) %>%
       #filter(treatment_name == 'high water + cage') %>%
       drop_na(indiv_count) %>%
       group_by(date,  habitat_type) %>% #site_id,
       summarise(sum(indiv_count)) %>%
       rename('total_arth' = 3) %>%
       arrange(date)
     })
     #treatment_title = df_final$treatment_name = 'high water + cage'
     # arth_treatment_select <- reactive({
     #   treatment_title = df_final$treatment_name == input$treatment_name
     #   })
   treatment_title <- reactive({
     df_final %>%
       filter(treatment_name == input$treatment_name) %>%
       select(treatment_name) %>%
       unique()
   })

   #widget3_arthropod_treatment_id plot
   output$arth_treatment_plot <- renderPlot({
     ggplot(data = arth_treatment_select(), aes(x = date, y = total_arth, colour = habitat_type)) +
       geom_line(aes(colour = habitat_type, group = habitat_type), size = 3) +
       #geom_boxplot(aes(colour = habitat_type, group = habitat_type)) +
       #geom_point(size = 2)+
       labs(x = 'Date', y = 'Total Count', colour = 'Habitat Type', title = str_wrap(paste0('Total arthropod count by month on brittlebush plants treated with ',treatment_title())))+
       scale_color_manual(values= wes_palette("GrandBudapest1", n = 3))+
       theme_minimal()+
       theme(axis.text.x=element_text(angle=45,hjust=1, size = 10))

   },bg = 'transparent')

   #widget4_arthropod_abundance_date data
   date_select <- reactive({
     df_final %>%
       select(month,month_number, plant_dry_mass, indiv_count) %>%
       group_by(month, month_number) %>%
       summarise(across(c(indiv_count, plant_dry_mass), ~ mean(.x, na.rm = TRUE))) %>%
       arrange(month_number)
   })
   #widget4_arthropod_abundance_date plot
   output$date_plot <- renderPlot({
     date_select() %>%
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
#use renderPlotly for interactive map with zoom etc!!!
   #in server, output$arth_treatment_plot <- renderPlotly({})
   #in ui, plotlyOutput(outputId = "habitat_plot")


#    #widget5_plant_biomass_date data
#    date_select2 <- reactive({
#      df_final %>%
#        select(month_number,plant_dry_mass, indiv_count) %>%
#        group_by(month_number, plant_dry_mass, indiv_count)
# })
#    #widget5_plant_biomass_date plot
#    output$date_plot2 <- renderPlot({
#      date_select2() %>%
#        ggplot()+
#        geom_boxplot(aes(x = month_number, y = plant_dry_mass, group = month_number, fill = factor(month_number)))+
#        coord_cartesian(xlim=input$date_slider)+
#        scale_fill_manual(values= c("#F1BB7B", "#FD6467", "#5B1A18", "#D67236","#A2A475","#FAEFD1"))+
#        labs(x = "Month Number", y = "Plant Biomass ()", fill = "Month Number")+
#        theme_minimal()
#    })
}





shinyApp(ui = ui, server = server)

