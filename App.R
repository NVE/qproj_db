# Libraries

if (.Platform$OS.type == "windows") {
  library('shiny')
  library('shinydashboard')
  library('ggplot2')
  library('lubridate')
  library('plotly')
  library('GetoptLong')
  library('leaflet')
  library('jsonlite')
  library('raster')
  library('rgdal')
}

if (.Platform$OS.type == "unix") {
  library('shiny')
  library('shinydashboard')
  library('ggplot2')
  library('lubridate')
  library('plotly')
  library('GetoptLong')
  library('leaflet', lib = "/home/jmg/R/x86_64-pc-linux-gnu-library/3.3")
  library('jsonlite')
  library('raster')
  library('rgdal')
}

# if (!require('shiny')) install.packages('shiny', repos = "http://cran.us.r-project.org"); library('shiny')
# if (!require('shinydashboard')) install.packages('shinydashboard', repos = "http://cran.us.r-project.org"); library('shinydashboard')
# if (!require('ggplot2')) install.packages('ggplot2', repos = "http://cran.us.r-project.org"); library('ggplot2')
# if (!require('lubridate')) install.packages('lubridate', repos = "http://cran.us.r-project.org"); library('lubridate')
# if (!require('plotly')) install.packages('plotly', repos = "http://cran.us.r-project.org"); library('plotly')
# if (!require('GetoptLong')) install.packages('GetoptLong', repos = "http://cran.us.r-project.org"); library('GetoptLong')
# if (!require('leaflet')) install.packages('leaflet', repos = "http://cran.us.r-project.org"); library('leaflet')
# if (!require('jsonlite')) install.packages('jsonlite', repos = "http://cran.us.r-project.org"); library('jsonlite')
# if (!require('raster')) install.packages('raster', repos = "http://cran.us.r-project.org"); library('raster')
# if (!require('rgdal')) install.packages('rgdal', repos = "http://cran.us.r-project.org"); library('rgdal')

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
                           width = 850,
                           height = 760),
             
             absolutePanel(id = "map_controls",
                           draggable = TRUE,
                           top = 10, left = "auto",
                           right = 40, bottom = "auto",
                           width = 150, height = "auto",
                           
                           selectInput(inputId = "map_pts",
                                       label = NULL,
                                       choices = c("Mean runoff", "Mean runoff (txt)", "Runoff efficiency"),
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
      
      addPolygons(data = data_monthly[[istat]]$wsh_poly,
                  weight = 2,
                  color = "red",
                  fill = FALSE,
                  opacity = 1,
                  layerId = "selected_poly") %>%
      
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
    
    if(input$mymap_marker_click$id != "selected_stat") {
      
      istat <- which(stats == input$mymap_marker_click$id)
      
      leafletProxy("mymap", session) %>%
        
        addPolygons(data = data_monthly[[istat]]$wsh_poly,
                    weight = 2,
                    color = "red",
                    fill = FALSE,
                    opacity = 1,
                    layerId = "selected_poly") %>%
        
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
  
  # This observer is responsible for updating the station markers
  
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
    
    # Markers display mean runoff without text
    
    if (map_pts == "Mean runoff") {
      
      # Colors and legend label
      
      pal <- colorBin(palette = "Blues",
                      na.color = "transparent",
                      bins = c(0, 500, 800, 1200, 1700, 2500, 3500, 5000, 7000))
      
      col_binned <- pal(df_meta$runoff_mean)
      
      # Update map
      
      leafletProxy("mymap") %>%
        
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
    
    # Markers display mean runoff with text
    
    if (map_pts == "Mean runoff (txt)") {
      
      # Colors and legend label
      
      pal <- colorBin(palette = "Blues",
                      na.color = "transparent",
                      bins = c(0, 500, 800, 1200, 1700, 2500, 3500, 5000, 7000))
      
      col_binned <- pal(df_meta$runoff_mean)
      
      # Update map
      
      leafletProxy("mymap") %>%
        
        addCircleMarkers(lng = df_meta$longitude,
                         lat = df_meta$latitude,
                         layerId = paste(df_meta$regine_area, df_meta$main_no, sep = "."),
                         fillColor = col_binned,
                         fillOpacity = 1,
                         stroke = TRUE,
                         weight = 1,
                         color = "black",
                         radius = 6,
                         opacity = 1,
                         label = as.factor(round(df_meta$runoff_mean, digits = 0)),
                         labelOptions = labelOptions(noHide = T,
                                                     textOnly = TRUE,
                                                     direction = 'right',
                                                     textsize='15px')) %>%
        
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
    
    plot_map(df_meta, data_monthly)
    
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