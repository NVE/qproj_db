# Libraries

library(shiny)
library(shinydashboard)

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
             tabBox(
               
               id = "tabset1", width = 12,
               
               tabPanel("Station map",
                        leafletOutput(outputId = "mymap",
                                      width = 520,
                                      height = 700)),
               tabPanel("Map covarage", plotOutput(outputId = "plot_wsh",
                                                   width = 520,
                                                   height = 700))
               
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