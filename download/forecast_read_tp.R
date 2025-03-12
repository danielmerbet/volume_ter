library(ncdf4)

homedir <- "~/Documents/intoDBP/volume_ter/download/forecast"
setwd(homedir)

month <- "11"
year <- 2023
members <- 51
horizon <- 215 #daily
res <- "24h"
var <- "tp"
method_inter <- "nearest"

target_lat <- 41.9702
target_lon <- 2.3994
lat <- ncvar_get(var_open, "latitude")  # [43.3, 42.3, 41.3]
lon <- ncvar_get(var_open, "longitude") # [1.5, 2.5, 3.5]

var_file <- paste0(var, "_", res, "_", month,"_", year,".nc")
var_open <- nc_open(var_file)
#hourly data
var_data <- ncvar_get(var_open, var)#order: longitude, latitude, time, member
#daily data
var_daily <- array(NA, dim = c(length(lon), length(lat), horizon, members))
var_daily[,,1,] <- var_data[,,1,]# First time step remains the same
var_daily[,,2:horizon,] <- var_data[,,2:horizon,] - var_data[,,1:(horizon-1),] # Compute daily differences for the remaining time steps
dim(var_daily)  # Should be [3, 3, 215, 51]

#interpolate to the lake location

if (method_inter == "nearest"){
  #find closest lat lon to the lake:
  lat_idx <- which.min(abs(lat - target_lat))# Find the index of the closest latitude
  lon_idx <- which.min(abs(lon - target_lon))# Find the index of the closest longitude
  var_daily_inter <- var_daily[lat_idx, lon_idx, , ]  # Keeping all days and ensemble members
}

if (method_inter =="bilinear"){
  # Find indices of the surrounding grid points
  lat_idx1 <- max(which(lat >= target_lat))  # Upper lat index
  lat_idx2 <- min(which(lat <= target_lat))  # Lower lat index
  lon_idx1 <- max(which(lon <= target_lon))  # Left lon index
  lon_idx2 <- min(which(lon >= target_lon))  # Right lon index
  
  # Extract the actual lat/lon values
  lat1 <- lat[lat_idx1]  # Upper latitude
  lat2 <- lat[lat_idx2]  # Lower latitude
  lon1 <- lon[lon_idx1]  # Left longitude
  lon2 <- lon[lon_idx2]  # Right longitude
  
  # Extract values at the four surrounding points
  Q11 <- var_daily[lat_idx1, lon_idx1, , ]  # Upper left
  Q12 <- var_daily[lat_idx1, lon_idx2, , ]  # Upper right
  Q21 <- var_daily[lat_idx2, lon_idx1, , ]  # Lower left
  Q22 <- var_daily[lat_idx2, lon_idx2, , ]  # Lower right
  
  # Compute weights for interpolation
  w_lat1 <- (lat2 - target_lat) / (lat2 - lat1)  # Weight for upper latitude
  w_lat2 <- 1 - w_lat1                           # Weight for lower latitude
  w_lon1 <- (lon2 - target_lon) / (lon2 - lon1)  # Weight for left longitude
  w_lon2 <- 1 - w_lon1                           # Weight for right longitude
  
  # Bilinear interpolation formula
  var_daily_inter <- w_lat1 * (w_lon1 * Q11 + w_lon2 * Q12) +
    w_lat2 * (w_lon1 * Q21 + w_lon2 * Q22)
  
  # Check dimensions: should be (215, 51) (days, ensemble members)
  dim(var_daily_inter)
}

colnames(var_daily_inter) <- paste0("member", 1:members)

#change to mm/day
var_daily_inter <- var_daily_inter*1000 

write.csv(var_daily_inter, file = paste0("../../in/", var, "_", month,"_", year,".csv"),
          quote = F, row.names = F)

png(paste0("../plot_forecast/", var,"_", res, "_", month,"_", year,".png"))
plot(var_daily_inter[,1], type="l", col="blue", ylim=c(0,100))
for (i in 2:members){
  lines(var_daily_inter[,i], col="blue")
}
dev.off()
