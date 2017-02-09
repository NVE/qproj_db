
# Data frame with coordinates of gauged areas

get_gauged_wsh <- function(data_monthly, xgrid, ygrid) {
  
  index_gauged <- c()
  
  for (iwsh in 1:length(data_monthly)) {
    
    index_gauged <- c(index_gauged, data_monthly[[iwsh]]$wsh_index)
    
  }
  
  index_gauged <- unique(index_gauged)
  
  df_gauged <- data.frame(utm_east_z33 = t(xgrid)[index_gauged],
                          utm_north_z33 = t(ygrid)[index_gauged],
                          value = rep(1, length(index_gauged)))
  
}

# Collect metadata

get_metadata <- function(data_list, stats_keep)  {
  
  # Load metadata from previous session if exist, otherwise load the original metadata
  
  if (file.exists("data/metadata_updated.txt")) {
    
    df_meta <- read.table("data/metadata_updated.txt", header = TRUE, sep = ";", dec = ".")
    
  } else {
    
    # Read metadata table
    
    df_meta <- read.table("data/metadata.txt", header = TRUE, sep = ";", dec = ".")
    
    # Augment with runoff efficiency
    
    df_meta$runoff_eff <- sapply(data_monthly, function(x) x$runoff_eff)
    
    # Augment with mean runoff
    
    df_meta$runoff_mean <- sapply(data_monthly, function(x) x$runoff_mean)
    
    # Assign data quality
    
    df_meta <- asses_data_qualtiy(df_meta)
    
  }
  
  return(df_meta)
  
}



# Assess data qualilty

asses_data_qualtiy <- function(df_meta) {
  
  df_meta$data_qual <- "poor_qual"
  
  # Watersheds with assumed low quality
  
  is_low_quality <- df_meta$regulation_part_area > 0 |
    df_meta$regulation_part_reservoirs > 0 |
    df_meta$transfer_area_in > 0 |
    df_meta$transfer_area_out > 0
  
  df_meta$data_qual[is_low_quality] <- "low_qual"
  
  # Watersheds with assumed good quality
  
  is_high_quality <- df_meta$br6_Klimastudier == "Y" |
    df_meta$br1_middelavrenning_1930_1960 == "Y" |
    df_meta$br23_HBV == "Y" |
    df_meta$br9_Flomvarsling == "Y"
  
  df_meta$data_qual[is_high_quality] <- "high_qual"
  
  return(df_meta)
  
}


# Function for averaging precipitation and air temperature over elevation bands

average_elevbands <- function(data_list) {
  
  # Number of elevation bands and time steps
  
  nbands <- length(data_list$frac_elev_band)
  
  ntimes <- length(data_list$time_vec)
  
  # Fraction of elevation bands to matrix
  
  frac_elev_band <- data_list$frac_elev_band
  
  frac_elev_band <- matrix(frac_elev_band, ntimes, nbands, byrow=TRUE)
  
  # Compute area averaged precipitation and air temperature
  
  Prec <- data_list$Prec
  
  data_list$Prec_mean <- rowSums(Prec * frac_elev_band)
  
  Tair <- data_list$Tair
  
  data_list$Tair_mean <- rowSums(Tair * frac_elev_band)
  
  return(data_list)
  
}


# Compute runoff efficiency

runoff_efficiency <- function(data_list) {
  
  df <- data.frame(prec = data_list$Prec_mean,
                   runoff = data_list$Runoff)
  
  df <- na.omit(df)
  
  data_list$runoff_eff <- sum(df$runoff)/sum(df$prec)
  
  return(data_list)
  
}


# Compute annual mean runoff

runoff_mean <- function(data_list) {
  
  data_list$runoff_mean <- 12*mean(data_list$Runoff, na.rm = TRUE)
  
  return(data_list)
  
}
