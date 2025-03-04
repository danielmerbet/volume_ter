library(lubridate); library(imputeTS)

dir <- "/home/dmercado/Documents/intoDBP/volume_ter/"
setwd(dir)

#out_option <- 1 #1: median last x days
out_option <- 2 #2: same as last similar season
min_vol <- 0.00 #minimum volume in percentage
max_vol <- 0.95

#initialisation forecast
date_ini <- as.Date("2024-10-01")
date_ini_previous <- as.Date("2023-10-01")
date_end_previous <- as.Date("2024-04-30")

#calculated balances
sau_balance <- read.csv("out/calculated_sau.csv")

#current balance for the actual dates
sau_balance_actual <- read.csv("in/water_balance_sau_actual.csv")

#load forecast sau
inflow_for_sau <- read.csv("out/inflow_for_sau.csv") 

#median outflows of the last x days
if (out_option==1){
  x_days <- 5
  dates_median_out <- seq(date_ini-x_days,date_ini-1, by=1)
  sau_balance$date <- as.Date(sau_balance$date)
  out_median_sau <- median(sau_balance[sau_balance$date %in% dates_median_out,]$Qout)
  print(paste0("SAU median outflow of last ", x_days, ": ",round(out_median_sau,2)))
  outflow <- rep(out_median_sau, nrow(inflow_for_sau))
}

if (out_option==2){
  sau_balance$date <- as.Date(sau_balance$date)
  #select previous season
  dates_previous <- seq(date_ini_previous, date_end_previous, by=1)
  sel_pos <- sau_balance$date %in% dates_previous
  pseas <- sau_balance[sel_pos,]
  print(paste0("SAU outflow from last similar season ", date_ini_previous, "-",date_end_previous))
  outflow <- pseas$Qout[1:nrow(inflow_for_sau)]
}

#Initial volume calculation SAU
V_ini_sau <- sau_balance[sau_balance$date==(date_ini-1),"V"]
members <- 51
change_Q_sau <- data.frame(matrix(NA,nrow(inflow_for_sau),members)); 
change_V_sau <- data.frame(matrix(NA,nrow(inflow_for_sau),members)); 
V_total_sau <- data.frame(matrix(NA,nrow(inflow_for_sau),members))
for (m in 1:members){
  # Qin - Qout
  change_Q_sau[,m] <- inflow_for_sau[,(1+m)] - outflow
  
  #daily volume change per day in hm3
  change_V_sau[,m] <- change_Q_sau[,m]*(86400/1e6)
  
  #daily total volume
  V_total_temp <- (V_ini_sau+change_V_sau[1,m])
  V_total_temp_vector <- V_total_temp
  for (d in 2:nrow(inflow_for_sau)){
    V_total_temp <- (V_total_temp+change_V_sau[d,m])
    V_total_temp_vector <- c(V_total_temp_vector, V_total_temp)
  }
  V_total_sau[,m] <- V_total_temp_vector

}

plot(V_total_sau[,1], type="l", ylim=c(-100,200))
for (i in 2:51){
  lines(V_total_sau[,i])
}

#Corrected volume calculation SAU
#There are some V negative and some other greater than the maximum
#we will limit the approach by:
#1. if the volume get lower than 5%, it convert into river, so Qin = Qout
#2. if the volume get higher than 95%, then also Qin = Qout


change_Q_sau <- data.frame(matrix(NA,nrow(inflow_for_sau),members)); 
change_V_sau <- data.frame(matrix(NA,nrow(inflow_for_sau),members)); 
V_total_sau <- data.frame(matrix(NA,nrow(inflow_for_sau),members))
Qout_sau <- data.frame(matrix(NA,nrow(inflow_for_sau),members))
for (m in 1:members){
  # Qin - Qout
  Qout_sau[,m] <- outflow
  change_Q_sau[,m] <- inflow_for_sau[,(1+m)] - Qout_sau[,m]
  
  #daily volume change per day in hm3
  change_V_sau[,m] <- change_Q_sau[,m]*(86400/1e6)
  
  #daily total volume
  V_total_temp <- (V_ini_sau+change_V_sau[1,m])
  #assume Qin=Qout when volumen is lower than min_vol 5% or greater than 95%
  if (V_total_temp<(unique(sau_balance$Vmax)*min_vol)){
    Qout_sau[1,m] <- inflow_for_sau[1,(1+m)]
    change_Q_sau[1,m] <- inflow_for_sau[1,(1+m)] - Qout_sau[1,m] #should be 0
    change_V_sau[1,m] <- change_Q_sau[1,m]*(86400/1e6)
    V_total_temp <- (V_ini_sau+change_V_sau[1,m])
  }
  if (V_total_temp>(unique(sau_balance$Vmax)*max_vol)){
    Qout_sau[1,m] <- inflow_for_sau[1,(1+m)]
    change_Q_sau[1,m] <- inflow_for_sau[1,(1+m)] - Qout_sau[1,m] #should be 0
    change_V_sau[1,m] <- change_Q_sau[1,m]*(86400/1e6)
    V_total_temp <- (V_ini_sau+change_V_sau[1,m])
  }
  V_total_temp_vector <- V_total_temp
  for (d in 2:nrow(inflow_for_sau)){
    V_total_temp <- (V_total_temp+change_V_sau[d,m])
    #assume Qin=Qout when volumen is lower than 5% or greater than 95%
    if (V_total_temp<(unique(sau_balance$Vmax)*min_vol)){
      V_total_temp <- (V_total_temp-change_V_sau[d,m])
      print(paste0("d: ", d, "m: ", m))
      Qout_sau[d,m] <- inflow_for_sau[d,(1+m)]
      print(paste0("Qin: ", inflow_for_sau[d,(1+m)]))
      print(paste0("Qout: ", Qout_sau[d,m]))
      change_Q_sau[d,m] <- inflow_for_sau[d,(1+m)] - Qout_sau[d,m] #should be 0
      print(change_Q_sau[d,m])
      change_V_sau[d,m] <- change_Q_sau[d,m]*(86400/1e6)
    }
    if (V_total_temp>(unique(sau_balance$Vmax)*max_vol)){
      V_total_temp <- (V_total_temp-change_V_sau[d,m])
      print(paste0("d: ", d, "m: ", m))
      Qout_sau[d,m] <- inflow_for_sau[d,(1+m)]
      change_Q_sau[d,m] <- inflow_for_sau[d,(1+m)] - Qout_sau[d,m] #should be 0
      change_V_sau[d,m] <- change_Q_sau[d,m]*(86400/1e6)
    }
    V_total_temp_vector <- c(V_total_temp_vector, V_total_temp)
  }
  V_total_sau[,m] <- V_total_temp_vector
  
}

pdf("plot/3_forecast_sau.pdf")
#plot corrected volumes
plot(as.Date(inflow_for_sau$date), V_total_sau[,1], type="l", 
     ylim=c(0,unique(sau_balance$Vmax)), ylab="Volume Sau (hm³)", xlab="Date")
for (i in 2:51){
  lines(as.Date(inflow_for_sau$date), V_total_sau[,i])
}
#ensemble mean
lines(as.Date(inflow_for_sau$date), rowMeans(V_total_sau), col="red", lwd=3)
#real dynamic previous year
dates_plot <- seq(as.Date("2023-10-01"), as.Date("2024-04-30"), by=1)
sel_pos <- sau_balance$date %in% dates_plot
#plot(sau_balance$date[sel_pos],sau_balance$V[sel_pos], type="l")
lines(as.Date(inflow_for_sau$date),sau_balance$V[sel_pos][1:length(inflow_for_sau$date)], 
      col="blue", lwd=3)
lines(as.Date(sau_balance_actual$date, format = "%m/%d/%Y"),sau_balance_actual$V, 
      col="green", lwd=3)
legend("topleft", legend=c("Ensemble", "Last season", "51 members", "Actual season"), 
       col=c("red","blue", "black","green"), lty=1, cex=0.8, bty="n")
dev.off()

png("plot/3_forecast_sau.png", width = 800, height = 600, units = "px")
#plot corrected volumes
plot(as.Date(inflow_for_sau$date), V_total_sau[,1], type="l", 
     ylim=c(0,unique(sau_balance$Vmax)), ylab="Volume Sau (hm³)", xlab="Date")
for (i in 2:51){
  lines(as.Date(inflow_for_sau$date), V_total_sau[,i])
}
#ensemble mean
lines(as.Date(inflow_for_sau$date), rowMeans(V_total_sau), col="red", lwd=3)
#real dynamic previous year
dates_plot <- seq(as.Date("2023-10-01"), as.Date("2024-04-30"), by=1)
sel_pos <- sau_balance$date %in% dates_plot
#plot(sau_balance$date[sel_pos],sau_balance$V[sel_pos], type="l")
lines(as.Date(inflow_for_sau$date),sau_balance$V[sel_pos][1:length(inflow_for_sau$date)], 
      col="blue", lwd=3)
lines(as.Date(sau_balance_actual$date, format = "%m/%d/%Y"),sau_balance_actual$V, 
      col="green", lwd=3)
legend("topleft", legend=c("Ensemble", "Last season", "51 members", "Actual season"), 
       col=c("red","blue", "black","green"), lty=1, cex=0.8, bty="n")
dev.off()


#save results forecast
write.csv(data.frame(date=inflow_for_sau$date, change_Q_sau), 
          file="out/forecast_sau/for_change_Q_sau.csv", 
          quote = F,row.names = F)
write.csv(data.frame(date=inflow_for_sau$date, change_V_sau),
          file="out/forecast_sau/for_change_V_sau.csv",
          quote = F,row.names = F)
write.csv(data.frame(date=inflow_for_sau$date, Qout_sau),
          file="out/forecast_sau/for_Qout_sau.csv",
          quote = F,row.names = F)
write.csv(data.frame(date=inflow_for_sau$date, V_total_sau),
          file="out/forecast_sau/for_V_sau.csv",
          quote = F,row.names = F)


