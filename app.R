#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(rjson)
library(leaflet)
library(shinydashboard)
library(leaflet.extras)
library(htmltools)
library(shinythemes)

df <- readRDS(file = "data.rds")
df_cities <- readRDS(file = 'cities.rds')
pallette = colorNumeric('YlOrRd', df_cities$number, reverse = T)

# UI####
#Initialize UI and set the theme
ui <- fluidPage(theme = shinytheme('sandstone'),
                tags$head(
                    tags$style(
                        HTML(
                        ".panel {
                        font-weight: 500;
                        color: #9e9e9e;
                        padding-left: 20px;
                        padding-right: 20px;
                        background-color: #1f1f1f;
                        }
                        h2 {
                        font-weight: bold;
                        "
                        ))),
                    
    shinyUI(
    
    navbarPage(title = 'Alternative Fuel Stations',
            
            # create tab panel to explore an interactive map of stations
            tabPanel(title = "Interactive Map",
                     
                    leafletOutput('map', width = '100%', height = 1000),
                    
                    # create draggable panel to control map parameters
                    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                  draggable = TRUE, top = 'auto', left = "auto", right = 20, bottom = 20,
                                  width = 330, height = 435,
                                  
                                  h2("Map Options"),
                                  
                                  # change whether to map individual stations or stations aggregated by city
                                  selectizeInput('groupBy', 'Group By:', c('Individual stations', 'City'), selected = 'City'),
                                  
                                  # change whether the points display as heatmap or individual points
                                  selectizeInput("plotType", "Plot Type", c('Locations', 'Heatmap')),
                                  
                                  # select which types of stations to display
                                  checkboxGroupInput("stationType", "Type of Fueling Station:", unique(df$Fuel.Type.Code), selected = 'Electric')
                                  )
                    ),
            
            #On the other tab, display a histogram of open dates, colored by fuel type
            #the idea is to add more functionality in the future in this tab
            tabPanel(title = 'Open Dates',
                     fluidRow(plotOutput('histogram', height = 800, width = "100%")))
            ))
        )

# Server ####
server <- function(input, output, session) {
    
    # select dataframe of values and lat/lng based on selectors in UI
    get_points <- reactive({
        if (input$groupBy == 'City') {
            
            columns_to_sum = names(cities)[names(cities) %in% input$stationType]
            
            cities_temp = cities %>% 
                ungroup() %>% 
                select(city, columns_to_sum, lat, lng, population, density)
             
            # find total number of stations in the city
            cities_temp$number = rowSums(cities_temp[,columns_to_sum])
            
            # arrange by number of stations so that the leaflet overlay renders the largest cities last
            cities_temp %>% arrange(number)
            
        } else if (input$groupBy == 'Individual stations'){
            
            df %>% 
                filter(Fuel.Type.Code %in% input$stationType) %>% 
                select(Latitude, Longitude)
        }
        
    })
    
    # render leaflet plot ####    
    # set prefer canvas = TRUE in order to shorten rendering times
    output$map = renderLeaflet({
        leaflet(points, width = '100%', height = '100%', options = leafletOptions(preferCanvas = T)) %>% 
            addProviderTiles(providers$CartoDB.DarkMatter, 
                             options = providerTileOptions(updateWhenIdle = T)) %>%
            setView(-94, 36, 5)
    })
    
    #observe function to update the leaflet overlays based on UI        
    observe({
        
        # grab a data frame to plot using the above reactive function
        points = get_points()
        
        # create leaflet overlays for both plotting options when displaying individual stations
        if (input$groupBy == 'Individual stations') {
            
            if (input$plotType == 'Heatmap') {
                
                # heatmap based on station location
                leafletProxy('map', data = points) %>% 
                    clearHeatmap() %>% clearMarkers() %>%
                    addHeatmap(lat = ~Latitude, lng = ~Longitude,
                               intensity = .025, blur = 23, max = 0.05, radius = 10, cellSize =8)
                
            } else if (input$plotType == 'Locations'){
                
                # slightly transparent circle markers for individual stations
                leafletProxy('map', data = points) %>% 
                    clearHeatmap() %>% clearMarkers() %>%
                    addCircleMarkers(lat = ~Latitude, lng = ~Longitude, 
                                     color = '#FFFF96', radius = 1, opacity = 0.3)
            } 
            
        } else if (input$groupBy == 'City') {
            
            # circle marker overlay for cities, with size and brightness corresponding to number of stations
            leafletProxy('map', data = points) %>% 
                clearHeatmap() %>% clearMarkers() %>%
                addCircleMarkers(lat = ~lat, lng = ~lng, 
                                 color = ~pallette(number), fillColor = ~pallette(number), 
                                 radius = ~number^0.8/5, 
                                 opacity = ~(number^0.25)/2.7, fillOpacity = ~(number^0.5)/20,
                                 
                                 # create a label when mousing over cities that displays the city name and number of stations
                                 label = ~htmlEscape(paste(city, ": ", number, sep = '')), 
                                 labelOptions = labelOptions(textOnly = T,
                                                             offset = c(25,-24),
                                                             direction = 'right',
                                                             style = list(
                                                                 "color" = "white",
                                                                 "font-size" = "14px",
                                                                 "font-family" = "Arial",
                                                                 "font-weight" = "bold",
                                                                 "font-style" = "italic"
                                                             )))
        }
        
    })
    
    # observe funcion for the second page to update charts
    observe({
        output$histogram = renderPlot(
            df %>% filter(Open.Date > '2000-01-01') %>% ggplot() +
                geom_histogram(aes(x = Open.Date, color = Fuel.Type.Code, fill = Fuel.Type.Code), binwidth = 50) +
                theme(
                    panel.grid.major = element_line(colour = "gray60"),
                    panel.grid.minor = element_line(colour = "gray60"),
                    axis.text = element_text(size = 18, colour = "gray40"),
                    axis.title = element_text(
                        size = 20,
                        face = "italic",
                        colour = "gray40"
                    ),
                    plot.title = element_text(
                        size = 30,
                        face = "bold",
                        colour = "darkorange2",
                        hjust = 0.5,
                        vjust = 1.75),
                    legend.title = element_text(colour = "gray40", size = 18),
                    legend.text = element_text(colour = "gray40", size = 14),
                    legend.key = element_rect(colour = "gray40"),
                    legend.background = element_rect(colour = "gray40"),
                    legend.position = c(0.13, 0.7),
                    panel.background = element_rect(fill = "gray35")) + 
                labs(title = "Alternative Fuel Stations over Time",
                         x = "Date the tation was opened", y = "Count")
                                                                               
        )
    })
    
    # update selectizeInput to allow heatmap only when individual stations are chosen
    observe({
        
        x = input$groupBy
        
        if (x== 'City') {
            updateSelectizeInput(session, 'plotType', choices = c('Locations'))
        } else if (x == 'Individual stations') {
            updateSelectizeInput(session, 'plotType', choices = c('Locations', 'Heatmap'))
        }
    })
}

# Run the application
shinyApp(ui = ui, server = server)


