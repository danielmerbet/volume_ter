library(lubridate); library(imputeTS)

#initialisation forecast
date_ini <- as.Date("2024-10-01")

#calculated balances
sqd_balance <- read.csv("out/calculated_sqd.csv")

#current balance for the actual dates
sqd_balance_actual <- read.csv("in/water_balance_sqd_actual.csv")

#median outflows of the last x days
x_days <- 10
dates_median_out <- seq(date_ini-x_days,date_ini-1, by=1)
sqd_balance$date <- as.Date(sqd_balance$date)
out_median_sqd <- median(sqd_balance[sqd_balance$date %in% dates_median_out,]$Qout)
print(paste0("SQD median outflow of last ", x_days, ": ",round(out_median_sqd,2)))

#outflow from sau = inflow for sqd
inflow_for_sqd<- read.csv("out/forecast_sau/for_Qout_sau.csv")

#Initial volume calculation sqd
V_ini_sqd <- sqd_balance[which(sqd_balance$date==(date_ini-1)),"V"]
members <- 51
change_Q_sqd <- data.frame(matrix(NA,nrow(inflow_for_sqd),members)); 
change_V_sqd <- data.frame(matrix(NA,nrow(inflow_for_sqd),members)); 
V_total_sqd <- data.frame(matrix(NA,nrow(inflow_for_sqd),members))
for (m in 1:members){
  # Qin - Qout
  change_Q_sqd[,m] <- inflow_for_sqd[,(1+m)] - rep(out_median_sqd, nrow(inflow_for_sqd))
  
  #daily volume change per day in hm3
  change_V_sqd[,m] <- change_Q_sqd[,m]*(86400/1e6)
  
  #daily total volume
  V_total_temp <- (V_ini_sqd+change_V_sqd[1,m])
  V_total_temp_vector <- V_total_temp
  for (d in 2:nrow(inflow_for_sqd)){
    V_total_temp <- (V_total_temp+change_V_sqd[d,m])
    V_total_temp_vector <- c(V_total_temp_vector, V_total_temp)
  }
  V_total_sqd[,m] <- V_total_temp_vector
  
}

plot(V_total_sqd[,1], type="l", ylim=c(-100,100))
for (i in 2:51){
  lines(V_total_sqd[,i])
}

#Corrected volume calculation sqd
#There are some V negative and some other greater than the maximum
#we will limit the approach by:
#1. if the volume get lower than 5%, it convert into river, so Qin = Qout
#2. if the volume get higher than 95%, then also Qin = Qout


change_Q_sqd <- data.frame(matrix(NA,nrow(inflow_for_sqd),members)); 
change_V_sqd <- data.frame(matrix(NA,nrow(inflow_for_sqd),members)); 
V_total_sqd <- data.frame(matrix(NA,nrow(inflow_for_sqd),members))
Qout_sqd <- data.frame(matrix(NA,nrow(inflow_for_sqd),members))
for (m in 1:members){
  # Qin - Qout
  Qout_sqd[,m] <- rep(out_median_sqd, nrow(inflow_for_sqd))
  change_Q_sqd[,m] <- inflow_for_sqd[,(1+m)] - Qout_sqd[,m]
  
  #daily volume change per day in hm3
  change_V_sqd[,m] <- change_Q_sqd[,m]*(86400/1e6)
  
  #daily total volume
  V_total_temp <- (V_ini_sqd+change_V_sqd[1,m])
  #assume Qin=Qout when volumen is lower than 5% or greater than 95%
  if (V_total_temp<(unique(sqd_balance$Vmax)*0.05)){
    Qout_sqd[1,m] <- inflow_for_sqd[1,(1+m)]
    change_Q_sqd[1,m] <- inflow_for_sqd[1,(1+m)] - Qout_sqd[1,m] #should be 0
    change_V_sqd[1,m] <- change_Q_sqd[1,m]*(86400/1e6)
    V_total_temp <- (V_ini_sqd+change_V_sqd[1,m])
  }
  if (V_total_temp>(unique(sqd_balance$Vmax)*0.95)){
    Qout_sqd[1,m] <- inflow_for_sqd[1,(1+m)]
    change_Q_sqd[1,m] <- inflow_for_sqd[1,(1+m)] - Qout_sqd[1,m] #should be 0
    change_V_sqd[1,m] <- change_Q_sqd[1,m]*(86400/1e6)
    V_total_temp <- (V_ini_sqd+change_V_sqd[1,m])
  }
  V_total_temp_vector <- V_total_temp
  for (d in 2:nrow(inflow_for_sqd)){
    V_total_temp <- (V_total_temp+change_V_sqd[d,m])
    #assume Qin=Qout when volumen is lower than 5% or greater than 95%
    if (V_total_temp<(unique(sqd_balance$Vmax)*0.05)){
      V_total_temp <- (V_total_temp-change_V_sqd[d,m])
      print(paste0("d: ", d, "m: ", m))
      Qout_sqd[d,m] <- inflow_for_sqd[d,(1+m)]
      print(paste0("Qin: ", inflow_for_sqd[d,(1+m)]))
      print(paste0("Qout: ", Qout_sqd[d,m]))
      change_Q_sqd[d,m] <- inflow_for_sqd[d,(1+m)] - Qout_sqd[d,m] #should be 0
      print(change_Q_sqd[d,m])
      change_V_sqd[d,m] <- change_Q_sqd[d,m]*(86400/1e6)
    }
    if (V_total_temp>(unique(sqd_balance$Vmax)*0.95)){
      V_total_temp <- (V_total_temp-change_V_sqd[d,m])
      Qout_sqd[1,m] <- inflow_for_sqd[d,(1+m)]
      change_Q_sqd[1,m] <- inflow_for_sqd[d,(1+m)] - Qout_sqd[d,m] #should be 0
      change_V_sqd[1,m] <- change_Q_sqd[d,m]*(86400/1e6)
    }
    V_total_temp_vector <- c(V_total_temp_vector, V_total_temp)
  }
  V_total_sqd[,m] <- V_total_temp_vector
  
}

pdf("plot/3_forecast_sqd.pdf")
#plot corrected volumes
plot(as.Date(inflow_for_sqd$date), V_total_sqd[,1], type="l", 
     ylim=c(0,unique(sqd_balance$Vmax)),  ylab="Volume SQD (hm³)", xlab="Date")
for (i in 2:51){
  lines(as.Date(inflow_for_sqd$date), V_total_sqd[,i])
}
#ensemble mean
lines(as.Date(inflow_for_sqd$date), rowMeans(V_total_sqd), col="red", lwd=3)
#real dynamic previous year
dates_plot <- seq(as.Date("2023-10-01"), as.Date("2024-04-30"), by=1)
sel_pos <- sqd_balance$date %in% dates_plot
#plot(sqd_balance$date[sel_pos],sqd_balance$V[sel_pos], type="l")
lines(as.Date(inflow_for_sqd$date),sqd_balance$V[sel_pos][1:length(inflow_for_sqd$date)], 
      col="blue", lwd=3)
lines(as.Date(sqd_balance_actual$date, format = "%m/%d/%Y"),sqd_balance_actual$V, 
      col="green", lwd=3)
legend("topleft", legend=c("Ensemble", "Last season", "51 members", "Actual season"), 
       col=c("red","blue", "black","green"), lty=1, cex=0.8, bty="n")
dev.off()

png("plot/3_forecast_sqd.png", width = 800, height = 600, units = "px")
#plot corrected volumes
plot(as.Date(inflow_for_sqd$date), V_total_sqd[,1], type="l", 
     ylim=c(0,unique(sqd_balance$Vmax)),  ylab="Volume SQD (hm³)", xlab="Date")
for (i in 2:51){
  lines(as.Date(inflow_for_sqd$date), V_total_sqd[,i])
}
#ensemble mean
lines(as.Date(inflow_for_sqd$date), rowMeans(V_total_sqd), col="red", lwd=3)
#real dynamic previous year
dates_plot <- seq(as.Date("2023-10-01"), as.Date("2024-04-30"), by=1)
sel_pos <- sqd_balance$date %in% dates_plot
#plot(sqd_balance$date[sel_pos],sqd_balance$V[sel_pos], type="l")
lines(as.Date(inflow_for_sqd$date),sqd_balance$V[sel_pos][1:length(inflow_for_sqd$date)], 
      col="blue", lwd=3)
lines(as.Date(sqd_balance_actual$date, format = "%m/%d/%Y"),sqd_balance_actual$V, 
      col="green", lwd=3)
legend("topleft", legend=c("Ensemble", "Last season", "51 members", "Actual season"), 
       col=c("red","blue", "black","green"), lty=1, cex=0.8, bty="n")
dev.off()

#save results forecast
write.csv(change_Q_sqd, file="out/forecast_sqd/for_change_Q_sqd.csv")
write.csv(change_V_sqd, file="out/forecast_sqd/for_change_V_sqd.csv")
write.csv(Qout_sqd, file="out/forecast_sqd/for_Qout_sqd.csv")
