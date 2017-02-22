# Script for preparing data to the application

rm(list=ls())

# Function for computing polygons surrouding watersheds

wsh_polygon <- function(data) {
  
  print(data$regine_main)
  
  # Read watershed indices
  
  wsh_index <- data$wsh_index
  
  wsh_index <- as.numeric(wsh_index) + 1
  
  # Create raster
  
  all_index <- seq(1, 1550 * 1195)
  
  all_index[all_index %in% wsh_index == FALSE] = NA
  all_index[all_index %in% wsh_index == TRUE] = 1
  
  tmp_raster <- raster(matrix(all_index, nrow = 1550, ncol = 1195, byrow = TRUE), xmn = -75000, xmx = 1120000, ymn = 6450000, ymx = 8000000)
  
  crs(tmp_raster) <- sp::CRS("+proj=utm +zone=33 +datum=WGS84")
  
  # Raster to polygon
  
  wsh_poly <- rasterToPolygons(tmp_raster, dissolve = TRUE)
  
  wsh_poly <- spTransform(wsh_poly, CRS("+init=epsg:4326"))
  
  # Augment to data
  
  data$wsh_poly <- wsh_poly
  
  return(data)
  
}

# Load data

load("//hdata/fou/Avrenningskart/Data/data_r_files/senorge_monthly_v20.RData")

load("//hdata/fou/Avrenningskart/Data/data_r_files/senorge_yearly_v20.RData")

# Read metadata

df_meta <- read.table("//hdata/fou/Avrenningskart/Data/metadata.txt", header = TRUE, sep = ";", dec = ".")

# Keep stations with at least 5 years of data

nr_obs <- sapply(data_monthly, function(x) sum(is.na(x$Runoff)==FALSE))

ikeep <- nr_obs > 12*5

data_monthly <- data_monthly[ikeep]

data_yearly <- data_yearly[ikeep]

df_meta <- df_meta[ikeep, ]

# Polygons for watersheds

data_monthly <- lapply(data_monthly, wsh_polygon)

# Save to files

save(data_monthly, file = "data/senorge_monthly_v20.RData")

save(data_yearly, file = "data/senorge_yearly_v20.RData")

write.table(df_meta, file = "data/metadata.txt", quote = FALSE, sep = ";", row.names = FALSE)

# Prepare precipitation map

library(lubridate)
library(raster)

stime <- head(data_daily[[1]]$time_vec, 1)
etime <- tail(data_daily[[1]]$time_vec, 1)

time_vec <- seq(stime, etime, by = "days")

prec_acc <- 0

for (iday in seq_along(time_vec)) {
  
  print(paste("Processing date:", time_vec[iday]))
  
  syear <- substr(time_vec[iday], 1, 4)
  smonth <- substr(time_vec[iday], 6, 7)
  sday <- substr(time_vec[iday], 9, 10)
  
  sdate <- paste(syear, smonth, sday, sep = "_")
  
  filename <- paste("//hdata/grid/metdata/met_obs_v2.1/rr/", syear, "/rr_", sdate, ".bil", sep = "")
  
  fid <- file(filename,"rb")
  prec_vec <- readBin(fid, integer(), n=1195*1550, size=2)
  close(fid)
  
  prec_vec[prec_vec == -1] <- NA
  
  prec_acc <- prec_acc + prec_vec/10
  
}

prec_acc <- 365 * prec_acc / length(time_vec)

prec <- matrix(prec_acc, nrow = 1550, ncol = 1195, byrow = TRUE)

prec_raster <- raster(prec, xmn = -75000, xmx = 1120000, ymn = 6450000, ymx = 8000000)

crs(prec_raster) <- sp::CRS("+proj=utm +zone=33 +datum=WGS84")

prec_raster <- projectRasterForLeaflet(prec_raster)

save(prec_raster, file = "data/prec_raster.RData")


# # Empty raster
# 
# empty_raster <- raster(matrix(NA, nrow = 1550, ncol = 1195), xmn = -75000, xmx = 1120000, ymn = 6450000, ymx = 8000000)
# 
# crs(empty_raster) <- sp::CRS("+proj=utm +zone=33 +datum=WGS84")
# 
# empty_raster <- projectRasterForLeaflet(empty_raster)
# 
# save(empty_raster, file = "data/empty_raster.RData")
















