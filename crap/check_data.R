
# Libraries

library("dplyr")

# Load r data

load("//hdata/fou/Avrenningskart/Data/data_r_files/senorge_daily_v20.RData")

# Select a station

regine <- "109"
main <- "9"

# Date range

xlim=c(as.Date('2000-01-01', format="%Y-%m-%d"),
       as.Date('2005-01-01', format="%Y-%m-%d"))

# Get r results

regine_main <- sapply(data_daily, function(x) x$regine_main)

istat <- which(regine_main == paste(regine, main, sep = "."))

date_r <- data_daily[[istat]]$time_vec
prec_r <- data_daily[[istat]]$Prec

# Get julia results

df_data <- read.table(paste("//unixhome/users/jmg/flood_forecasting/model_input/", paste(regine, main, sep = "_"), "_data/Prec.txt", sep = ""), sep = ";")
df_meta <- read.table(paste("//unixhome/users/jmg/flood_forecasting/model_input/", paste(regine, main, sep = "_"), "_data/metadata.txt", sep = ""), sep = ";", header = TRUE)

frac <- df_meta$area / sum(df_meta$area)
frac <- matrix(frac, nrow(df_data), length(frac), byrow = TRUE)

date_j <- as.Date(df_data[,1])
prec_j <- rowSums(as.matrix(df_data[,2:ncol(df_data)])*frac)

plot(date_j, prec_j, xlim = xlim)
lines(date_r, prec_r, col = "red", xlim = xlim)


# Get runoff data

data <- data.frame(date = data_daily[[istat]]$time_vec,
                   runoff = data_daily[[istat]]$Runoff)

date_start <-as.Date("2012-01-01")  
date_stop <-as.Date("2012-12-31")

test<- subset(data, date >= date_start & date <= date_stop)

head(test)

















