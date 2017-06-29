
# Helper code

source("data_handling.R")
source("plotting.R")

# Process data

load("data/senorge_monthly_v20.RData")

# load("data/senorge_daily_v20.RData")
# 
# data_monthly <- data_daily

data_monthly <- lapply(data_monthly, average_elevbands)

data_monthly <- lapply(data_monthly, comp_stats)

# Get station names

get_stat <- function(x) x$regine_main

stats <- sapply(data_monthly, get_stat)

# Get metadata

df_meta <- get_metadata(data_monthly)

# Grids with coordinates

xvec <- seq(from = -75000, to = 1119000, by = 1000)
yvec <- seq(from = 7999000, to = 6450000, by = -1000)

xgrid <- matrix(xvec, nrow = 1550, ncol = 1195, byrow = TRUE)
ygrid <- matrix(yvec, nrow = 1550, ncol = 1195, byrow = FALSE)

# # Data frame with gauged areas
# 
# df_gauged <- get_gauged_wsh(data_monthly, xgrid, ygrid)

# Load rasters

load("data/prec_raster.RData")
