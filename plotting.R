

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
    xlab("Runoff (mm)") + 
    ylab("Precipitation (mm)")
  
}

# Plot runoff time series

plot_runoff <- function(data_monthly, istat) {
  
  name <- paste(data_monthly[[istat]]$metadata$regine_main,
                data_monthly[[istat]]$metadata$station_name, sep = " ")
  
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

# Plot map

plot_map <- function(df_meta) {
  
  # Colors for runoff efficiency
  
  cbPalette <- c('#5060E8', '#91bfdb','#fee090','#fc8d59','#d73027')
  
  colors_qeff <- cut(df_meta$runoff_eff,
                     c(0, 0.8, 1.2, 1.6, 2.0, 1000.0),
                     labels = cbPalette)
  
  values <- c("< 0.8", "0.8 - 1.2", "1.2 - 1.6", "1.6 - 2.0", "> 2.0")
  
  # Colors for mean runoff
  
  cbPalette <- c('#5060E8', '#91bfdb','#fee090','#fc8d59','#d73027')
  
  colors_mean <- cut(df_meta$runoff_mean,
                     c(0, 800, 1300, 1800, 2500, 10000.0),
                     labels = cbPalette)
  
  # Colors for precipitation map
  
  pal <- colorBin(palette = "Blues",
                  domain = getValues(prec_raster),
                  na.color = "transparent",
                  bins = 7)
  
  # Create leaflet map
  
  leaflet() %>%
    
    addProviderTiles("Stamen.TonerLite",
                     options = providerTileOptions(noWrap = TRUE)) %>%
    
    addRasterImage(prec_raster, colors = pal, opacity = 0.8, group = "Precipitation") %>%
    
    addCircleMarkers(group = "Runoff efficiency",
                     lng = df_meta$longitude,
                     lat = df_meta$latitude,
                     layerId = paste(df_meta$regine_area, df_meta$main_no, sep = "."),
                     color = colors_qeff,
                     radius = 6,
                     stroke = FALSE,
                     opacity = 1,
                     fillOpacity = 1) %>%
    
    addCircleMarkers(group = "Mean runoff",
                     lng = df_meta$longitude,
                     lat = df_meta$latitude,
                     layerId = paste(df_meta$regine_area, df_meta$main_no, sep = "."),
                     color = colors_mean,
                     radius = 6,
                     stroke = FALSE,
                     opacity = 1,
                     fillOpacity = 1) %>%
    
    addCircleMarkers(lng = df_meta$longitude[1],
                     lat = df_meta$latitude[1],
                     layerId = "selected_stat",
                     color = "black",
                     radius = 8,
                     stroke = TRUE,
                     opacity = 1,
                     fillOpacity = 0) %>%
    
    addLegend("bottomright",
              colors = cbPalette,
              labels = values,
              #group = "Double",
              title = "Runoff efficiency") %>%
    
    fitBounds(min(df_meta$longitude),
              min(df_meta$latitude),
              max(df_meta$longitude),
              max(df_meta$latitude)) %>%
    
    addLayersControl(baseGroups = c("Mean runoff", "Runoff efficiency"),
                     overlayGroups = c("Precipitation"))
  
}

# Plot gauged areas

plot_gauged_area <- function(df_gauged) {
  
  ggplot(df_gauged, aes(x = utm_east_z33, y = utm_north_z33)) + 
    geom_raster(aes(fill = value), show.legend = FALSE) +
    theme_classic(base_size = 10) + 
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
    theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
    coord_fixed(ratio = 1)+ coord_cartesian()
  
}


wsh_polygon_id <- function(x) {
  
  stat_name <- x$properties$Name
  
  stat_name <- strsplit(stat_name, " ")
  
  stat_name <- tail(stat_name[[1]],1)
  
  stat_name <- gsub(".0", "", stat_name, fixed = TRUE)
  
}





