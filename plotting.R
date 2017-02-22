

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

# Plot map

plot_map <- function(df_meta, data_monthly) {
  
  # Colors for precipitation map
  
  pal <- colorBin(palette = "Blues",
                  na.color = "transparent",
                  bins = c(0, 500, 800, 1200, 1700, 2500, 3500, 5000, 7000))
  
  # Create leaflet map
  
  leaflet() %>%
    
    addTiles("http://opencache.statkart.no/gatekeeper/gk/gk.open_gmaps?layers=topo2graatone&zoom={z}&x={x}&y={y}",
             attribution = "© Kartverket") %>%
    
    #addProviderTiles("Stamen.TonerLite", options = providerTileOptions(noWrap = TRUE)) %>%
    
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





