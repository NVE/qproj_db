

# Plot station markers on the map

plot_markers <- function(map_disp, df_meta) { #, id_selected) {
  
  
  
  # 
  # # Markers display selected stations
  # 
  # if (map_disp == "Selected stations") {
  #   
  #   # Colors and legend label
  #   
  #   col_pal <- c('#5ff442', '#f71f07')
  #   
  #   col_breaks <- c(-Inf, 0.5, Inf)
  #   
  #   legend_str <- c("Selected", "Discarded")
  #   
  #   col_binned <- cut(id_selected, col_breaks, labels = col_pal)
  #   
  #   # Update map
  #   
  #   leafletProxy("stat_map") %>%
  #     
  #     addCircleMarkers(lng = df_meta$longitude,
  #                      lat = df_meta$latitude,
  #                      layerId = paste(df_meta$regine_area, df_meta$main_no, sep = "."),
  #                      color = col_binned,
  #                      radius = 6,
  #                      stroke = FALSE,
  #                      opacity = 1,
  #                      fillOpacity = 1) %>%
  #     
  #     addLegend("bottomright",
  #               colors = col_pal,
  #               labels = legend_str,
  #               title = "Selected stations",
  #               layerId = "stat_legend") %>%
  #     
  #     hideGroup(group = "Precipitation")
  #   
  # }
  # 
  
  
  
  
  
  # Markers display runoff efficiency
  
  if (map_disp == "Runoff efficiency") {
    
    # Colors and legend label
    
    col_pal <- c('#5060E8', '#91bfdb','#fee090','#fc8d59','#d73027')
    
    col_breaks <- c(0, 0.8, 1.2, 1.6, 2.0, Inf)
    
    legend_str <- c("< 0.8", "0.8 - 1.2", "1.2 - 1.6", "1.6 - 2.0", "> 2.0")
    
    col_binned <- cut(df_meta$runoff_eff, col_breaks, labels = col_pal)
    
    # Update map
    
    leafletProxy("stat_map") %>%
      
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
  
  if (map_disp == "Mean runoff") {
    
    # Restrict range
    
    runoff_tmp <- df_meta$runoff_mean
    
    cat(paste("Number of stations with runoff > 7000 mm/year:", sum(runoff_tmp>7000), "\n"))
    
    runoff_tmp[runoff_tmp>7000] <- 7000
    
    # Colors and legend label
    
    pal <- colorBin(palette = "Blues",
                    na.color = "transparent",
                    bins = c(0, 500, 800, 1200, 1700, 2500, 3500, 5000, 7000))
    
    col_binned <- pal(runoff_tmp)
    
    # Update map
    
    leafletProxy("stat_map") %>%
      
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
                values = runoff_tmp,
                title = "Mean runoff",
                layerId = "stat_legend") %>%
      
      showGroup(group = "Precipitation")
    
  }
  
  
  # Markers display mean runoff with text
  
  if (map_disp == "Mean runoff (txt)") {
    
    # Restrict range
    
    runoff_tmp <- df_meta$runoff_mean
    
    cat(paste("Number of stations with runoff > 7000 mm/year:", sum(runoff_tmp>7000)))
    
    runoff_tmp[runoff_tmp>7000] <- 7000
    
    # Colors and legend label
    
    pal <- colorBin(palette = "Blues",
                    na.color = "transparent",
                    bins = c(0, 500, 800, 1200, 1700, 2500, 3500, 5000, 7000))
    
    col_binned <- pal(runoff_tmp)
    
    # Update map
    
    leafletProxy("stat_map") %>%
      
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
                       label = as.factor(round(runoff_tmp, digits = 0)),
                       labelOptions = labelOptions(noHide = T,
                                                   textOnly = TRUE,
                                                   direction = 'right',
                                                   textsize='15px')) %>%
      
      addLegend("bottomright",
                pal = pal,
                values = runoff_tmp,
                title = "Mean runoff",
                layerId = "stat_legend") %>%
      
      showGroup(group = "Precipitation")
    
  }
  
}


# Plot cumulative precipitation against runoff

plot_cumsums <- function(data_monthly, istat) {
  
  df <- data.frame(runoff = data_monthly[[istat]]$Runoff,
                   prec = data_monthly[[istat]]$Prec_mean)
  
  df <- na.omit(df)
  
  runoff_acc <- cumsum(df$runoff)
  
  prec_acc <- cumsum(df$prec)
  
  df <- data.frame(runoff_acc = runoff_acc, prec_acc = prec_acc)
  
  ggplot(data = df, aes(x = runoff_acc, y = prec_acc)) +
    geom_smooth(method = 'lm', se = FALSE, size = 1, linetype = 2, col = "red") +
    geom_point() +
    xlab("Cumulative runoff (mm)") + 
    ylab("Cumulative precipitation (mm)")
  
}


# Plot runoff time series (note that this plot often produces warnings
# since the time series can contain missing values)

plot_runoff <- function(data_monthly, istat) {
  
  name <- paste(data_monthly[[istat]]$metadata$regine_main,
                data_monthly[[istat]]$metadata$station_name, sep = " - ")
  
  time <- ymd(data_monthly[[istat]]$time_vec)
  
  runoff <- data_monthly[[istat]]$Runoff
  
  df <- data.frame(time = time, runoff = runoff)
  
  ggplot(data = df, aes(x = time, y = runoff)) + 
    geom_line(col = "red", size = 1) +
    xlab("") +
    ylab("Runoff (mm/month)") +
    ggtitle(label = name) + 
    theme_set(theme_grey(base_size = 12)) 
  
}


# Plot the basic map

plot_map <- function(df_meta, data_monthly) {
  
  # Colors for precipitation map
  
  pal <- colorBin(palette = "Blues",
                  na.color = "transparent",
                  bins = c(0, 500, 800, 1200, 1700, 2500, 3500, 5000, 7000))
  
  # Create leaflet map
  
  leaflet() %>%
    
    addTiles("http://opencache.statkart.no/gatekeeper/gk/gk.open_gmaps?layers=topo2graatone&zoom={z}&x={x}&y={y}",
             attribution = "© Kartverket") %>%
    
    addRasterImage(prec_raster,
                   colors = pal,
                   opacity = 0.7,
                   group = "Precipitation",
                   project=FALSE) %>%
    
    fitBounds(min(df_meta$longitude),
              min(df_meta$latitude),
              max(df_meta$longitude),
              max(df_meta$latitude))
  
}







# # Plot gauged areas
# 
# plot_gauged_area <- function(df_gauged) {
#   
#   ggplot(df_gauged, aes(x = utm_east_z33, y = utm_north_z33)) + 
#     geom_raster(aes(fill = value), show.legend = FALSE) +
#     theme_classic(base_size = 10) + 
#     theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
#     theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
#     coord_fixed(ratio = 1)+ coord_cartesian()
#   
# }


# wsh_polygon_id <- function(x) {
#   
#   stat_name <- x$properties$Name
#   
#   stat_name <- strsplit(stat_name, " ")
#   
#   stat_name <- tail(stat_name[[1]],1)
#   
#   stat_name <- gsub(".0", "", stat_name, fixed = TRUE)
#   
# }





