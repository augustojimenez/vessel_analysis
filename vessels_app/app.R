# Importing libraries
suppressPackageStartupMessages({
    library(rgdal)
    library(readr)
    library(shiny)
    library(dplyr)
    library(leaflet)
    library(shiny.semantic)
})

# Dataframe containing the data to be displayed
ships_df <- read_csv("./data/ships_df.csv")

# Dataframe containing the data to feed the types of vessel dropdown
ship_types <- read_csv("./data/ship_types.csv") %>%
    arrange(ship_type) %>%
    .$ship_type

# Dataframe containing the data to feed the vessel's name dropdown
ships <- read_csv("./data/ship_list.csv") %>%
    arrange(ship_type, ship_id, ship_name)

myGridTemplate <- grid_template(
    default = list(
        areas = rbind(c("title", "shiptype", "shipname"),
                      c("card", "card", "card"),
                      c("map", "map", "map")),
        cols_width = c("30%", "35%", "auto"),
        rows_height = c("100px", "100px", "auto")),
    mobile = list(areas = rbind("title",
                                "shiptype",
                                "shipname",
                                "card",
                                "map"),
                  rows_height = c("50px", "50px", "50px", "50px", "auto"),
                  cols_width = c("100%")))

ui <- shinyUI(semanticPage(
    grid(myGridTemplate,
         title = h1("Distance Sailed by Ships"),
         shiptype = selectInput("shiptype_dd",
                                "Select Vessel Type:",
                                ship_types,
                                selected = ship_types[1],
                                width = "90%"),
         shipname = selectInput("shipname_dd",
                                "Select Vessel:",
                                ships$ship_name,
                                selected = ships$ship_name[1]),
         card = cards(
             class = "three",
             card(
                 div(class="content",
                     div(class="header", "Starting Point"),
                     div(class="meta", uiOutput("start_time")),
                     div(class="description", uiOutput("start_point"))
                 )
             ),
             card(
                 div(class="content",
                     div(class="header", uiOutput("c_distance")),
                     div(class="meta", uiOutput("c_weight")),
                     div(class="description", uiOutput("c_speed"))
                 )
             ),
             card(
                 div(class="content",
                     div(class="header", "Ending Point"),
                     div(class="meta", uiOutput("end_time")),
                     div(class="description", uiOutput("end_point"))
                 )
             )
         ),
         map = leafletOutput("ships_map",
                             width = "100%",
                             height = "100%"))))


server <- shinyServer(function(input, output, session) {
    observeEvent(input$shiptype_dd, {
        x <- ships %>%
            filter(ship_type == input$shiptype_dd) %>%
            unique() %>%
            .$ship_name
        
        updateSelectInput(session, "shipname_dd",
                          label = "Select Vessel:",
                          choices = x,
                          selected = x[1])
    })
    
    output$ships_map <- renderLeaflet({
        # Filters data to re-render map
        datos <- ships_df %>%
            filter(ship_name == input$shipname_dd)
        
        # Coordinates to center and re-zoom the view around the navigated area
        centroid_lon <- mean(datos$lon, na.rm = TRUE)
        centroid_lat <- mean(datos$lat, na.rm = TRUE)
        
        lat_min <- min(datos$lat, na.rm = TRUE)
        lat_max <- max(datos$lat, na.rm = TRUE)
        
        lon_min <- min(datos$lon, na.rm = TRUE)
        lon_max <- max(datos$lon, na.rm = TRUE)
        
        # Re-sizes circles' radius given the distance between the starting and
        # ending points
        rad <- sqrt((lon_max - lon_min)^2 + (lat_max - lat_min)^2)
        
        # Setting labels to fill the information cards
        start_time <- as.character(datos[[2, "dt"]])
        start_point <- paste0("(",
                              datos[datos$period == "prev", "lat"],
                              ", ",
                              datos[datos$period == "prev", "lon"],
                              ")")
        end_time <- as.character(datos[[1, "dt"]])
        end_point <- paste0("(",
                            datos[datos$period == "curr", "lat"],
                            ", ",
                            datos[datos$period == "curr", "lon"],
                            ")")
        distance <- paste("Navigated",
                          datos[[1, "distance"]] %>%
                              round(0) %>%
                              format(big.mark = ","),
                          "meters")
        weight <- datos[[1, "dwt"]]
        weight <- ifelse(is.na(weight) | weight == 0,
                         "Deadweight is not available",
                         paste("Deadweight of",
                               weight %>%
                                   round(0) %>%
                                   format(big.mark = ","),
                               "tons"))
        speed <- mean(datos$speed)
        speed <- ifelse(is.na(speed) | speed == 0,
                        "Average speed is not available",
                        paste("Average speed of",
                              speed %>%
                                  round(0) %>%
                                  format(big.mark = ","),
                              "knots"))
        
        ### Rendering Cards ###
        # Card Left
        output$start_time <- renderText({start_time})
        output$start_point <- renderText({start_point})
        
        # Card Centre
        output$c_distance <- renderText({distance})
        output$c_weight <- renderText({weight})
        output$c_speed <- renderText({speed})
        
        # Card Right
        output$end_time <- renderText({end_time})
        output$end_point <- renderText({end_point})
        
        content <- paste0("<b><a>", input$shipname_dd, "</a></b>", "<br/>",
                          "<strong>Distance: </strong>",
                          format(round(datos[[1, "distance"]],0),
                                 big.mark = ","), "<br/>",
                          "<strong>Point: </strong>", c(start_point, end_point), "<br/>",
                          "<strong>Speed: </strong>", datos$speed, "<br/>")
        ### Rendering Map ###
        leaflet() %>%
            clearMarkers() %>%
            setView(lng = centroid_lon,
                    lat = centroid_lat,
                    zoom = 0) %>%
            addProviderTiles("CartoDB.Positron") %>%
            addCircles(data = datos,
                       ~lon, ~lat,
                       radius = rad * 1000,
                       color = c("#F60D1D", "#0000FF"),
                       popup = content) %>%
            addLegend("topright",
                      colors = c("#F60D1D", "#0000FF", "grey"),
                      labels = c(paste0("<strong> Starting point </strong>"),
                                 paste0("<strong> Ending point </strong>"),
                                 paste0("<strong> Distance sailed </strong>")),
                      values = ~period,
                      opacity = 1) %>%
            fitBounds(lng1 = lon_max * 1.5, lat1 = lat_max * 1.5,
                      lng2 = lon_min * 1.5, lat2 = lat_min * 1.5) %>%
            clearBounds() %>%
            addPolylines(data = datos, ~lon, ~lat,
                         group = ~period, weight = 1,
                         color = "grey", stroke = TRUE)
    })
    
})

shinyApp(ui = ui, server = server)
