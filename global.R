
# Helper code

source("data_handling.R")
source("plotting.R")
source("printing.R")

# Process data

load("data/senorge_main.RData")

data_main <- lapply(data_main, comp_stats)

# Get station names

get_stat <- function(x) x$regine_main

stats <- sapply(data_main, get_stat)

# Get metadata

df_meta <- metadata_for_app(data_main)

# Grids with coordinates

xvec <- seq(from = -75000, to = 1119000, by = 1000)
yvec <- seq(from = 7999000, to = 6450000, by = -1000)

xgrid <- matrix(xvec, nrow = 1550, ncol = 1195, byrow = TRUE)
ygrid <- matrix(yvec, nrow = 1550, ncol = 1195, byrow = FALSE)

# Load rasters

load("data/prec_raster.RData")
