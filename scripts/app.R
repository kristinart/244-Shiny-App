
# This is a Shiny web application. You can run the application by clicking the 'Run App' button above.

library(shiny)
library(tidyverse)
library(here)
library(shinythemes)
library(bslib)
library(cowplot)

df_final <- read_csv(here('data','df_final.csv'))

ui <- fluidPage(
  # theme = bs_theme(
  #   version = 5,
  #   bootswatch = NULL,
  #   bg = "white",
  #   fg = "black",
  #   primary = "purple",
  #   secondary = "turquoise",
  #   success = "green",
  #   info = "dodgerblue",
  #   warning = "yellow",
  #   danger = "red",
  #   base_font = font_google("Merriweather"),
  #   code_font = font_google("Asar"),
  #   heading_font = font_google("Gravitas One"),
  #   font_scale = 1
  #), ### end of theme
  navbarPage(title = "Brittlebush Productivity and Arthropod Community Characteristics",
                 tabPanel(title = "Background",
                 fluidPage(
                   titlePanel("Introduction and Background"),
                   img(src ="https://www.researchgate.net/profile/Christofer-Bang/publication/225081502/figure/fig2/AS:669081560707081@1536532883563/Map-of-the-Phoenix-metropolitan-area-with-approximate-location-of-the-two-weather.ppm"),
                   p("This study examined the species abundance, richness, and evenness of arthropods, and the plant productivity of brittlebush, in response to different habitats and treatment conditions. The purpose of the study was to better understand any potential impact of different habitat types and growing conditions on urban biodiversity."),
                   sidebarLayout(
                     sidebarPanel(
                       (checkboxGroupInput("habitat_type",
                                     label = "Choose habitat type",
                                     choices = c("Desert", "Remnant", "Urban"))
                       ) #end of checkboxGroup
                     ), #end of sidebar panel
                     mainPanel("Output: image and description of habitat",
                               plotOutput(outputId = "habitat_image")
                               ) #end of main panel
                 ) #end of sidebar layout
                 ) #end of fluid page
                 ), #end of tab 1

                 tabPanel("Brittlebush",
                 fluidPage(titlePanel("Brittlebush Productivity Under Varying Conditions"),
                          p("Insert blurb on productivity of the plants under the various treatments"),
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons(inputId = "treatment_name",
                                                 label = "Select Plant water treatment",
                                                 unique(df_final$treatment_name))
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
                              Planning to clean up and pretty both plots. For top plot, planning to remove 12-1-2008 (or check if
                              that was actually 12-2007?? I think it might be because this data is such a mess). For bottom plot, hoping
                              to get a second y-axis to display plant dry mass too; since they are of such different magnitudes,
                              it doesn't look good to have them plotted on the same axis. Also hoping to get the slider widget and
                              x-axis of the plot to have month names (Jan Feb Mar, etc) as the tick labels. Lastly, open to displaying
                              something else on this tab...we originally talked about species richness but idrk if I want to try to
                              calculate that with the unclear labels in this dataset..."),
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("treatment_name",
                                            "Select cluster treatment",
                                            choices = unique(df_final$treatment_name)),
                                sliderInput(inputId = "date_slider",
                                            label = "Select month range",
                                            min = 1,
                                            max = 6,
                                            value = c(1,6),
                                            step = 1)
                              ), #end of sidebar panel
                              mainPanel(plotOutput(outputId = "arth_treatment_plot"),
                                        plotOutput(outputId = "date_plot"))
                              ) #end of sidebar layout
                            ) #end of fluidpage
                 ) #end of tab 3
                 )#end of navbarPage
)

server <-function(input, output, session){
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



  #
  # widget2_plant_treatment_type data
  treatment_select <- reactive({
    df_final %>%
      filter(treatment_name == input$treatment_name) #%>%
      #drop_na(plant_dry_mass) %>%
      #group_by(treatment_name) %>%
      #summarize(mean(plant_dry_mass)) #%>%
      #rename('avg_plant_mass' = 3)
  })

  #widget2_plant_treatment_type plot
  output$plant_treatment_plot <- renderPlot({
    ggplot(data = treatment_select(),
           aes(x = treatment_name,
               y = plant_dry_mass)) +
      geom_boxplot() +
      labs(x = "water + cage treatment",
           y = "plant dry mass",
           title = "Brittlebush Growth Resulting from Different Treatments") +
      theme_minimal()

  })

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
       select(treatment_name)
   })

   #widget3_arthropod_treatment_id plot
   output$arth_treatment_plot <- renderPlot({
     ggplot(data = arth_treatment_select(), aes(x = date, y = total_arth, colour = habitat_type)) +
       geom_line(aes(colour = habitat_type, group = habitat_type)) +
       #geom_boxplot(aes(colour = habitat_type, group = habitat_type)) +
       geom_point(size = 1.5)+
       labs(x = 'Date', y = 'Total Count', colour = 'Habitat Type', title = paste0('Total arthropod count by month on brittlebush plants treated with ','treatment_title'))+
       theme_minimal()

   })

   # # widget4_arthropod_abundance_date data
   date_select <- reactive({
     df_final %>%
       select(month_number,plant_dry_mass, indiv_count) %>%
       group_by(month_number) %>%
       summarise(across(indiv_count, ~ mean(.x, na.rm = TRUE)))
   })

   #widget4_arthropod_abundance_date plot
   output$date_plot <- renderPlot({
     date_select() %>%
       ggplot()+
       geom_line(aes(x = month_number, y = indiv_count))+
       coord_cartesian(xlim=input$date_slider)+
       labs(x = "Month Number", y = "Average arthropod count per plant")+
       theme_minimal()

   })

}





shinyApp(ui = ui, server = server)

