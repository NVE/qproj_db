
# Print summary table

print_summary <- function(data_monthly, istat) {
  
  # Station attributes
  
  station_name <- data_monthly[[istat]]$metadata$station_name
  area         <- data_monthly[[istat]]$metadata$area_total
  
  area <- round(area, digits = 0)
  
  # Runoff efficieny (runoff/precipitation)
  
  df <- data.frame(runoff = data_monthly[[istat]]$Runoff,
                   prec = data_monthly[[istat]]$Prec_mean)
  
  df <- na.omit(df)
  
  pq_ratio <- sum(df$runoff)/sum(df$prec)
  
  pq_ratio <- round(pq_ratio, digits = 1)
  
  # Easting and northing
  
  easting <- data_monthly[[istat]]$metadata$utm_east_z33
  northing <- data_monthly[[istat]]$metadata$utm_north_z33
  
  # Artificial influence
  
  is_regulated <- c(data_monthly[[istat]]$metadata$regulation_part_area,
                    data_monthly[[istat]]$metadata$regulation_part_reservoirs,
                    data_monthly[[istat]]$metadata$transfer_area_in,
                    data_monthly[[istat]]$metadata$transfer_area_out)
  
  if (any(is.na(is_regulated))) {
    is_regulated = "Unknown"
  } else if (any(is_regulated > 0)) {
    is_regulated = "Yes"
  } else {
    is_regulated = "No"
  }
  
  # Land use
  
  perc_forest <- round(data_monthly[[istat]]$metadata$perc_forest)
  perc_lake <- round(data_monthly[[istat]]$metadata$perc_lake)
  perc_glacier <- round(data_monthly[[istat]]$metadata$perc_glacier)
  
  # Bruksområden
  
  br1_middelavrenning_1930_1960 <- data_monthly[[istat]]$metadata$br1_middelavrenning_1930_1960
  br2_Tilsigsberegning <- data_monthly[[istat]]$metadata$br2_Tilsigsberegning
  br3_Regional_flomfrekvensanalyse <- data_monthly[[istat]]$metadata$br3_Regional_flomfrekvensanalyse
  br5_Regional_lavvannsanalyse <- data_monthly[[istat]]$metadata$br5_Regional_lavvannsanalyse
  br6_Klimastudier <- data_monthly[[istat]]$metadata$br6_Klimastudier
  br7_Klimascenarier <- data_monthly[[istat]]$metadata$br7_Klimascenarier
  br9_Flomvarsling <- data_monthly[[istat]]$metadata$br9_Flomvarsling
  br11_FRIEND <- data_monthly[[istat]]$metadata$br11_FRIEND
  br23_HBV <- data_monthly[[istat]]$metadata$br23_HBV
  br24_middelavrenning_1930_1960 <- data_monthly[[istat]]$metadata$br24_middelavrenning_1930_1960
  br26_TotalAvlop <- data_monthly[[istat]]$metadata$br26_TotalAvlop
  br31_FlomserierPrim <- data_monthly[[istat]]$metadata$br31_FlomserierPrim
  br32_FlomserierSekundar <- data_monthly[[istat]]$metadata$br32_FlomserierSekundar
  br33_Flomkart_aktive_ureg <- data_monthly[[istat]]$metadata$br33_Flomkart_aktive_ureg
  br34_Flomkart_aktive_ureg <- data_monthly[[istat]]$metadata$br34_Flomkart_aktive_ureg
  br38_Flomkart_aktive_ureg_periode <- data_monthly[[istat]]$metadata$br38_Flomkart_aktive_ureg_periode
  br39_Flomkart_nedlagt_stasjon <- data_monthly[[istat]]$metadata$br39_Flomkart_nedlagt_stasjon
  
  # Merge strings
  
  str <- c("@{station_name}\n",
           "Runoff/Prec = @{pq_ratio}\n",
           "Area = @{area} km2\n",
           "Easting = @{easting}\n",
           "Northing = @{northing}\n",
           "Is regulated = @{is_regulated}\n",
           "Forest = @{perc_forest} %\n",
           "Lake = @{perc_lake} %\n",
           "Glacier = @{perc_glacier} %\n",
           "Bruksområden\n",
           "Middelavrenning 1930-1960 = @{br1_middelavrenning_1930_1960}\n",
           "Tilsigsberegning = @{br2_Tilsigsberegning}\n",
           "Regional flomfrekvensanalyse = @{br3_Regional_flomfrekvensanalyse}\n",
           "Regional lavvannsanalyse = @{br5_Regional_lavvannsanalyse}\n",
           "Klimastudier = @{br6_Klimastudier}\n",
           "Klimascenarier = @{br7_Klimascenarier}\n",
           "Flomvarsling = @{br9_Flomvarsling}\n",
           "FRIEND = @{br11_FRIEND}\n",
           "HBV = @{br23_HBV}\n",
           "Middelavrenning_1930_1960 = @{br24_middelavrenning_1930_1960}\n",
           "TotalAvlop = @{br26_TotalAvlop}\n",
           "FlomserierPrim = @{br31_FlomserierPrim}\n",
           "FlomserierSekundar = @{br32_FlomserierSekundar}\n",
           "Flomkart_aktive_ureg = @{br33_Flomkart_aktive_ureg}\n",
           "Flomkart_aktive_ureg = @{br34_Flomkart_aktive_ureg}\n",
           "Flomkart_aktive_ureg_periode = @{br38_Flomkart_aktive_ureg_periode}\n",
           "Flomkart_nedlagt_stasjon = @{br39_Flomkart_nedlagt_stasjon}\n")
  
  qqcat(paste(str, collapse = ""))
  
}






















