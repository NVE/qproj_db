# Handle libraries on Windows and Linux

if (.Platform$OS.type == "windows") {
  library('dplyr')
  library('readxl')
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
  library('dplyr')
  library('readxl')
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

if (!require('dplyr')) install.packages('shiny', repos = "http://cran.us.r-project.org"); library('dplyr')
if (!require('readxl')) install.packages('shiny', repos = "http://cran.us.r-project.org"); library('readxl')

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


# Add global variables  ----------------------------------------------------

source("global.R")


# User interface -----------------------------------------------------------

ui <- dashboardPage(
  
  
  # Dashboard header
  
  dashboardHeader(title = "Runoff data check"),
  
  
  # Dashboard sidebar
  
  dashboardSidebar(
    
    width = 270,
    
    # Dropdown menu for station selection
    
    selectInput(inputId = "stat_dropdown",
                label = "Select station:",
                choices = stats,
                selected = paste(df_meta$regine_area[1], df_meta$main_no[1], sep = "."),
                multiple = FALSE,
                selectize = FALSE),
    
    # Summary table of station properties
    
    verbatimTextOutput(outputId = "print_summary")
    
  ),
  
  
  # Dashboard body
  
  dashboardBody(
    
    fluidRow(
      
      
      # One column and two rows with time series and mass balance plots
      
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
      
      
      # One column with map
      
      column(width = 5,
             
             leafletOutput(outputId = "stat_map",
                           height = 750),
             
             absolutePanel(id = "map_controls",
                           draggable = FALSE,
                           top = 10, left = "auto",
                           right = 40, bottom = "auto",
                           width = 150, height = "auto",
                           
                           selectInput(inputId = "map_disp",
                                       label = NULL,
                                       choices = c("Mean runoff", "Mean runoff (txt)", "Runoff efficiency", "Selected stations"),
                                       selected = "Mean runoff")
                           
             )
             
      )
      
    )
    
  )
  
)


# Server -------------------------------------------------------------------

server <- function(input, output, session) { 
  
  
  # This observer updates the station selection and catchment boundary in map
  # when selecting a new station
  
  observeEvent(input$stat_dropdown, {
    
    istat <- which(stats == input$stat_dropdown)
    
    leafletProxy("stat_map", session) %>%
      
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
  
  
  # This observer links the selection in the map with the selection in the dropdown menu
  
  observeEvent(input$stat_map_marker_click, {
    
    if(input$stat_map_marker_click$id != "selected_stat") {
      
      istat <- which(stats == input$stat_map_marker_click$id)
      
      leafletProxy("stat_map", session) %>%
        
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
      
      updateSelectInput(session, "stat_dropdown",
                        label = "Select station:",
                        choices = stats,
                        selected = input$stat_map_marker_click$id)
      
    }
    
  })
  
  
  # This observer is responsible for updating station markers when selecting
  # type of display in the map (for example "Mean runoff" or "Mean runoff (txt)")
  
  observe({
    
    map_disp <- input$map_disp
    
    plot_markers(map_disp, df_meta)
    
  })
  
  
  # Plot cumulative precipitation against cumulative runoff
  
  output$plot_cumsums <- renderPlot({
    
    istat <- which(stats == input$stat_dropdown)
    
    plot_cumsums(data_monthly, istat)
    
  })
  
  
  # Plot runoff time series
  
  output$plot_runoff <- renderPlot({
    
    istat <- which(stats == input$stat_dropdown)
    
    plot_runoff(data_monthly, istat)
    
  })
  
  
  # Plot map with stations
  
  output$stat_map <- renderLeaflet({
    
    plot_map(df_meta, data_monthly)
    
  })
  
  
  # Print summary table
  
  output$print_summary <- renderPrint({
    
    istat <- which(stats == input$stat_dropdown)
    
    print_summary(data_monthly, istat)
    
  })
  
}

shinyApp(ui, server)