# Libraries

library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(plotly)
library(GetoptLong)
library(leaflet)
library(jsonlite)
library(raster)

source("global.R")

# User interface -----------------------------------------------------------

ui <- dashboardPage(
  
  # Dashboard header
  
  dashboardHeader(title = "Runoff data check"),
  
  # Dashboard sidebar
  
  dashboardSidebar(
    
    width = 270,
    
    selectInput(inputId = "stat",
                label = "Select station:",
                choices = stats,
                selected = paste(df_meta$regine_area[1], df_meta$main_no[1], sep = "."),
                multiple = FALSE,
                selectize = FALSE),
    
    radioButtons("data_qualtiy", "Data quality:",
                 c("High (model calibration)" = "high_qual",
                   "Low (precipitation correction)" = "low_qual",
                   "Poor (station not useful)" = "poor_qual"),
                 selected = df_meta$data_qual[1]),
    
    verbatimTextOutput(outputId = "print_summary")
    
  ),
  
  # Dashboard body
  
  dashboardBody(
    
    fluidRow(
      column(width = 7,
             box(
               title = NULL,
               width = NULL,
               solidHeader = TRUE,
               status = "primary",
               plotOutput("plot_runoff",
                          height = 300)
             ),
             box(
               title = NULL,
               width = NULL,
               solidHeader = TRUE,
               status = "primary",
               plotOutput("plot_cumsums")
             )
             
      ),
      
      column(width = 5,
             
             leafletOutput(outputId = "mymap",
                           width = 650,
                           height = 760),
             
             absolutePanel(id = "map_controls",
                           draggable = TRUE,
                           top = 10, left = "auto",
                           right = 40, bottom = "auto",
                           width = 150, height = "auto",
                           
                           selectInput(inputId = "map_pts",
                                       label = NULL,
                                       choices = c("Mean runoff","Runoff efficiency"),
                                       selected = "Mean runoff")
                           
             )
             
      )
      
    )
    
  )
  
)


# Server -------------------------------------------------------------------

server <- function(input, output, session) { 
  
  # Reactive values for data qualtiy
  
  reactive_data <- reactiveValues(data_qual = df_meta$data_qual)
  
  # Update radio button for data quality
  
  observeEvent(input$data_qualtiy , {
    
    istat <- which(stats == input$stat)
    
    reactive_data$data_qual[istat] <- input$data_qualtiy
    
    df_meta$data_qual[istat] <- input$data_qualtiy
    
  })
  
  observeEvent(input$stat, {
    
    istat <- which(stats == input$stat)
    
    updateRadioButtons(session, 
                       inputId = "data_qualtiy",
                       label = "Data quality:",
                       choices = c("High (model calibration)" = "high_qual",
                                   "Low (precipitation correction)" = "low_qual",
                                   "Poor (station not useful)" = "poor_qual"),
                       selected = reactive_data$data_qual[istat])
    
    leafletProxy("mymap", session) %>%
      addCircleMarkers(lng = df_meta$longitude[istat],
                       lat = df_meta$latitude[istat],
                       layerId = "selected_stat",
                       color = "black",
                       radius = 8,
                       stroke = TRUE,
                       opacity = 1,
                       fillOpacity = 0)
    
  })
  
  # Link station selection in map and dropdown menu
  
  observeEvent(input$mymap_marker_click, {
    
    if(input$mymap_marker_click != "selected_stat") {
      
      ishape <- which(hbv_shape_id == input$mymap_marker_click$id)
      
      if (length(ishape) > 0) {
        
        leafletProxy("mymap", session) %>%
          addGeoJSON(hbv_shape$features[[ishape]],
                     weight = 1,
                     color = "#444444",
                     fill = FALSE,
                     opacity = 1,
                     layerId = "selected_poly")
        
      } else {
        leafletProxy("mymap", session) %>%
          removeGeoJSON(layerId = "selected_poly")
      }
      
      istat <- which(stats == input$mymap_marker_click$id)
      
      leafletProxy("mymap", session) %>%
        addCircleMarkers(lng = df_meta$longitude[istat],
                         lat = df_meta$latitude[istat],
                         layerId = "selected_stat",
                         color = "black",
                         radius = 8,
                         stroke = TRUE,
                         opacity = 1,
                         fillOpacity = 0)
      
      updateSelectInput(session, "stat",
                        label = "Select station:",
                        choices = stats,
                        selected = input$mymap_marker_click$id)
      
    }
    
  })
  
  # This observed is responsible for updating the station markers
  
  observe({
    
    map_pts <- input$map_pts
    
    # Markers display runoff efficiency
    
    if (map_pts == "Runoff efficiency") {
      
      # Colors and legend label
      
      col_pal <- c('#5060E8', '#91bfdb','#fee090','#fc8d59','#d73027')
      
      col_breaks <- c(0, 0.8, 1.2, 1.6, 2.0, Inf)
      
      legend_str <- c("< 0.8", "0.8 - 1.2", "1.2 - 1.6", "1.6 - 2.0", "> 2.0")
      
      col_binned <- cut(df_meta$runoff_eff, col_breaks, labels = col_pal)
      
      # Update map
      
      leafletProxy("mymap") %>%
        
        clearShapes() %>%
        
        addCircleMarkers(lng = df_meta$longitude,
                         lat = df_meta$latitude,
                         layerId = paste(df_meta$regine_area, df_meta$main_no, sep = "."),
                         color = col_binned,
                         radius = 6,
                         stroke = FALSE,
                         opacity = 1,
                         fillOpacity = 1) %>%
        
        addLegend("bottomright",
                  colors = col_pal,
                  labels = legend_str,
                  title = "Runoff efficiency",
                  layerId = "stat_legend") %>%
        
        hideGroup(group = "Precipitation")
      
    }
    
    # Markers display mean runoff
    
    if (map_pts == "Mean runoff") {
      
      # Colors and legend label
      
      pal <- colorBin(palette = "Blues",
                      na.color = "transparent",
                      bins = c(0, 500, 700, 1000, 1300, 1600, 2000, 2500, 3000, 4000, 6000))
      
      col_binned <- pal(df_meta$runoff_mean)
      
      # Update map
      
      leafletProxy("mymap") %>%
        
        clearShapes() %>%
        
        addCircleMarkers(lng = df_meta$longitude,
                         lat = df_meta$latitude,
                         layerId = paste(df_meta$regine_area, df_meta$main_no, sep = "."),
                         fillColor = col_binned,
                         fillOpacity = 1,
                         stroke = TRUE,
                         weight = 1,
                         color = "black",
                         radius = 6,
                         opacity = 1) %>%
        
        addLegend("bottomright",
                  pal = pal,
                  values = df_meta$runoff_mean,
                  title = "Mean runoff",
                  layerId = "stat_legend") %>%
        
        showGroup(group = "Precipitation")
      
    }
    
  })
  
  
  
  
  
  
  
  
  # Plot cumulative precipitation against runoff
  
  output$plot_cumsums <- renderPlot({
    
    istat <- which(stats == input$stat)
    
    plot_cumsums(data_monthly, istat)
    
  })
  
  # Plot runoff time series
  
  output$plot_runoff <- renderPlot({
    
    istat <- which(stats == input$stat)
    
    plot_runoff(data_monthly, istat)
    
  })
  
  # Plot map with stations
  
  output$mymap <- renderLeaflet({
    
    plot_map(df_meta)
    
  })
  
  # Plot map with gauged areas
  
  output$plot_wsh <- renderPlot({
    
    plot_gauged_area(df_gauged)
    
  })
  
  # Print summary table
  
  output$print_summary <- renderPrint({
    
    istat <- which(stats == input$stat)
    
    print_summary(data_monthly, istat)
    
  })
  
  # Save data quality assessment when exiting the app
  
  session$onSessionEnded(function() {
    df_meta$data_qual <- isolate(reactive_data$data_qual)
    write.table(df_meta, file = "data/metadata_updated.txt", quote = FALSE, sep = ";", row.names = FALSE)
  })
  
}

shinyApp(ui, server)