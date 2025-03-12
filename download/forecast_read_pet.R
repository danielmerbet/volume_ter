library(ncdf4); library(lubridate)

homedir <- "~/Documents/intoDBP/volume_ter/download/forecast"
setwd(homedir)

month <- "11"
year <- 2023
members <- 51
horizon <- 215 #daily
res <- "6h"
var <- "t2m"
method_inter <- "nearest"

target_lat <- 41.9702
target_lon <- 2.3994
var_file <- paste0(var, "_", res, "_", month,"_", year,".nc")
var_open <- nc_open(var_file)
lat <- ncvar_get(var_open, "latitude")  # [43.3, 42.3, 41.3]
lon <- ncvar_get(var_open, "longitude") # [1.5, 2.5, 3.5]

#hourly data
var_data <- ncvar_get(var_open, var)#order: longitude, latitude, time, member
var_data <- var_data - 273.15 #Kelvin to C
dim(var_data)
#daily temperatures (mean, min, max) data
dim_new <- c(dim(var_data)[1], dim(var_data)[2], 4, dim(var_data)[3] / 4, dim(var_data)[4])
var_mean <- apply(array(var_data, dim_new), c(1,2,4,5), mean, na.rm = TRUE)
var_max <- apply(array(var_data, dim_new), c(1,2,4,5), max, na.rm = TRUE)
var_min <- apply(array(var_data, dim_new), c(1,2,4,5), min, na.rm = TRUE)
#potential evaporation
dates <- seq(from = as.Date(paste0(year,'-', month,'-01')), by = 1, len = dim(var_mean)[3])
compute_Ra <- function(date, phi) {
  doy <- yday(date)  # Day of year
  
  # Constants
  Gsc <- 0.0820  # MJ/m²/min (solar constant)
  
  # Compute dr (inverse relative distance Earth-Sun)
  dr <- 1 + 0.033 * cos((2 * pi / 365) * doy)
  
  # Compute solar declination (delta)
  delta <- 0.409 * sin((2 * pi / 365) * doy - 1.39)
  
  # Compute sunset hour angle (omega_s)
  omega_s <- acos(-tan(phi) * tan(delta))
  
  # Compute Ra (MJ/m²/day)
  Ra <- (24 * 60 / pi) * Gsc * dr * 
    (omega_s * sin(phi) * sin(delta) + cos(phi) * cos(delta) * sin(omega_s))
  
  return(Ra)
}
phi <- target_lat * pi / 180  # Convert to radians
Ra_values <- sapply(dates, function(date) compute_Ra(date, phi))
hargreaves_samani <- function(tmax, tmin, Ra) {
  Ra_array <- array(rep(Ra, each = length(lat) * length(lon) * members), 
                    dim = c(length(lon), length(lat), horizon, members))
  #tmean <- (tmax + tmin) / 2
  #PET <- 0.0023 * (tmean + 17.8) * sqrt(tmax - tmin) * Ra
  trng <- tmax - tmin
  trng[trng < 0] <- 0  # Avoid negative values
  PET <- 0.0023 * (Ra_array / 2.45) * ((tmax + tmin) / 2 + 17.8) * sqrt(trng)
  return(PET)
}
PET_array <- hargreaves_samani(var_max, var_min, Ra_values)
var_daily <- PET_array
range(var_daily)

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

write.csv(var_daily_inter, file = paste0("../../in/", "pet_", month,"_", year,".csv"),
          quote = F, row.names = F)

png(paste0("../plot_forecast/", "pet_", res, "_", month,"_", year,".png"))
plot(var_daily_inter[,1], type="l", ylim=c(0,8))
for (i in 2:members){
  lines(var_daily_inter[,i])
}
dev.off()