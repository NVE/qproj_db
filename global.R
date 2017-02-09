
# Helper code

source("data_handling.R")
source("plotting.R")
source("printing.R")

# Process data

load("data/senorge_monthly_v20.RData")

data_monthly <- lapply(data_monthly, average_elevbands)

data_monthly <- lapply(data_monthly, runoff_efficiency)

data_monthly <- lapply(data_monthly, runoff_mean)

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

# Data frame with gauged areas

df_gauged <- get_gauged_wsh(data_monthly, xgrid, ygrid)

# Process shape files

hbv_shape <- readLines("data/hbv_catchments.json") %>% paste(collapse = "\n") %>% fromJSON(simplifyVector = FALSE)

hbv_shape_id <- sapply(hbv_shape$features, wsh_polygon_id)


# Test with raster

# library("raster")
# library("sp")

load("data/prec_raster.RData")


# DEM <- raster("data/myrprosent.asc")
# crs(DEM) <- sp::CRS("+proj=utm +zone=33 +datum=WGS84")
