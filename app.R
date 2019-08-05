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
ui <- fluidPage(theme = shinytheme('sandstone'), shinyUI(
    
    navbarPage(title = 'Alternative Fuel Stations',
            
            tabPanel(title = "Interactive Map",
                    leafletOutput('map', width = '100%', height = 1000),
                    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                  draggable = TRUE, top = 'auto', left = "auto", right = 20, bottom = 20,
                                  width = 330, height = "auto",
                                  
                                  h2("Map Options"),
                                  
                                  selectizeInput('groupBy', 'Group By:', c('Individual stations', 'City'), selected = 'City',
                                                 width = '80%'),
                                  selectizeInput("plotType", "Plot Type", c('Locations', 'Heatmap')),
                                  checkboxGroupInput("stationType", "Type of Fueling Station:", unique(df$Fuel.Type.Code), selected = 'Electric')
                                  )
                    ),
            
            tabPanel(title = 'Data Explorer',
                    plotOutput('histogram', width = "50%"))
            ))
        )

# Server ####
server <- function(input, output) {
    
    get_points <- reactive({
        if (input$groupBy == 'City') {
            
            columns_to_sum = names(cities)[names(cities) %in% input$stationType]
            
            cities_temp = cities %>% 
                ungroup() %>% 
                select(city, columns_to_sum, lat, lng, population, density)
             
            cities_temp$number = rowSums(cities_temp[,columns_to_sum])
            
            cities_temp %>% arrange(number)
            
        } else if (input$groupBy == 'Individual stations'){
            df %>% 
                filter(Fuel.Type.Code %in% input$stationType) %>% 
                select(Latitude, Longitude)
        }
        
    })
    
    #render leaflet plot ####    
    output$map = renderLeaflet({
        leaflet(points, width = '100%', height = '100%', options = leafletOptions(preferCanvas = T)) %>% 
            addProviderTiles(providers$CartoDB.DarkMatter, 
                             options = providerTileOptions(updateWhenIdle = T)) %>%
            setView(-94, 36, 5)
    })
            
    observe({
        
        points = get_points()
        
        if (input$groupBy == 'Individual stations') {
            
            if (input$plotType == 'Heatmap') {
                
                leafletProxy('map', data = points) %>% 
                    clearHeatmap() %>% clearMarkers() %>%
                    addHeatmap(lat = ~Latitude, lng = ~Longitude,
                               intensity = .025, blur = 23, max = 0.05, radius = 10, cellSize =8)
                
            } else if (input$plotType == 'Locations'){
                
                leafletProxy('map', data = points) %>% 
                    clearHeatmap() %>% clearMarkers() %>%
                    addCircleMarkers(lat = ~Latitude, lng = ~Longitude, 
                                     color = '#FFFF96', radius = 1, opacity = 0.3)
            } 
        } else if (input$groupBy == 'City') {
            
            leafletProxy('map', data = points) %>% 
                clearHeatmap() %>% clearMarkers() %>%
                addCircleMarkers(lat = ~lat, lng = ~lng, 
                                 color = ~pallette(number), fillColor = ~pallette(number), 
                                 radius = ~number^0.8/5, 
                                 opacity = ~(number^0.3)/4, fillOpacity = ~(number^0.5)/20,
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
    
    observe({
        output$histogram = renderPlot(
            df %>% filter(Open.Date>'2000-01-01') %>% ggplot() + 
                geom_histogram(aes(x = Open.Date, color = Fuel.Type.Code, fill = Fuel.Type.Code ), binwidth = 50)
        )
    })
}

# Run the application
shinyApp(ui = ui, server = server)


