library(shiny)
library(ggvis)
library(ggplot2)
library(tidyr)
library(dplyr)
library(raster)
library(rgdal)
library(rgeos)

#Load Socio Statistical Data
state.statistics <- read.csv("state.statistics.csv")
state.statistics.rank <- data.frame(Population.Growth.Rate = rank(state.statistics[,4], ties.method='min'),
                                    apply(-state.statistics[,5:7],2,rank, ties.method='min'),
                                    apply(state.statistics[,8:14],2,rank, ties.method='min'),
                                    Mean.Female.marriage.age = rank(-state.statistics[,15], ties.method='min'))
rownames(state.statistics.rank) <- state.statistics$State 
state.statistics$State <- factor(state.statistics$State)


#Load India Map Data
india <- getData("GADM", country = "India", level = 1)
map <- fortify(india)
map$id <- as.integer(map$id)
centers <- data.frame(id = 1:(length(india@data$NAME_1)), 
                  State = india@data$NAME_1)

map_df <- inner_join(map, centers, by = "id")
#Merge with Socio Statistical Data to ignore small States and Union Territories
map_df2 <- left_join(map_df, state.statistics, by = "State")

#Identify Center Point of each State to display State name
centers <- data.frame(gCentroid(india, byid = TRUE), 
                      State = centers$State)
centers <- left_join(state.statistics, centers, by = "State")



# Load State Literacy Data and Gather and filter for plotting
state.literacy <- read.csv("state.literacy.csv")
state.literacy <- state.literacy %>% 
  gather("Year","Literacy",-States)
state.literacy$Year <- gsub("X", "", state.literacy$Year)
state.literacy$Year <- as.numeric(state.literacy$Year)

#Ignore records with data as NA
state.literacy <- filter(state.literacy, !is.na(Literacy))

shinyServer(function(input, output) {
  
    #Render ToolTip
    all_values <- function(x) {
      if(is.null(x)) return(NULL)
      paste0(names(x), ": ", format(x), collapse = "<br />") }
    
    #Render Literacy Interactive Graph
    state.literacy.reactive <- reactive({
    state.literacy %>%
      filter(Year <= input$bins) %>% 
      ggvis(~Year, ~Literacy, stroke = ~States) %>% 
      layer_points(fill = ~States) %>% 
      layer_lines() %>% 
      add_axis("x", values = seq(1951, 2011, by = 10)) %>% 
      scale_numeric("x", domain = c(1951, 2011), nice = TRUE) %>%
      scale_numeric("y", domain = c(0, 100), nice = TRUE) %>%
      add_tooltip(all_values, ifelse(input$checkbox,"hover","click"))
   
    }) %>% 
      bind_shiny("distPlot", "plot_ui")
    
    #Render Bar Plot
    output$statsPlot <- renderPlot({
      stat.data <- arrange_(state.statistics, c(input$variable))
      par(mfrow = c(1, 1), mar = c(4, 5, 1, 1), oma = c(2,3,2,3))
      barplot(stat.data[, input$variable],
                  horiz = TRUE,
                  names.arg = stat.data$State,
                  col=c("darkblue"),
                  xlab=input$variable, las=1, 
                  xlim = range(pretty(c(0, stat.data[, input$variable]))))
    })
    
    #Render Map Plot
    output$mapsPlot <- renderPlot({
     ggplot() +
       geom_map(data = map_df2, map = map_df,
                aes_string(map_id = "id", 
                           x = "long", 
                           y = "lat", 
                           group = "group", 
                           fill = input$variable),
                color = "#ffffff", size = 0.25) +
       geom_text(data = centers, 
                 aes(label = State, 
                     x = x, 
                     y = y), 
                 size = 3) +
       coord_map() +
       labs(x = "", 
            y = "", 
            title = "Map of India")
    })
    
    #Render Data Plot
    output$dataTable = renderDataTable({
      df <-data.frame(state.statistics$State, state.statistics[, input$variable])
      colnames(df) <- c("State",input$variable)
      df
    })
    
    #Render State dropdown
    output$choose_states <- renderUI({
      selectInput("States", 
                  "state", 
                  as.list(rownames(state.statistics.rank)))
    })
    
    #Render Data Plot
    output$rankTable = renderDataTable({
      df <-data.frame(colnames(state.statistics.rank),as.numeric(as.vector(state.statistics.rank[input$States,])))
      colnames(df) <- c("Demographic Factor", "Rank")
      df
    })
})

