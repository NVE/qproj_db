# This is a temporary script

rm(list=ls())

# Load data

load("//hdata/fou/Avrenningskart/Data/data_r_files/senorge_daily_v20.RData")

load("//hdata/fou/Avrenningskart/Data/data_r_files/senorge_monthly_v20.RData")

load("//hdata/fou/Avrenningskart/Data/data_r_files/senorge_yearly_v20.RData")

# Read metadata

df_meta <- read.table("//hdata/fou/Avrenningskart/Data/metadata.txt", header = TRUE, sep = ";", dec = ".")

# Keep stations with at least 5 years of data

nr_obs <- sapply(data_monthly, function(x) sum(is.na(x$Runoff)==FALSE))

ikeep <- nr_obs > 12*5

data_daily <- data_daily[ikeep]

data_monthly <- data_monthly[ikeep]

data_yearly <- data_yearly[ikeep]

df_meta <- df_meta[ikeep, ]

# Save to files

save(data_daily, file = "data/senorge_daily_v20.RData")

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

save(prec_raster, file = "data/prec_raster.RData")





