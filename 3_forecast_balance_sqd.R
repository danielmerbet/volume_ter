#This code provides two outcomes: 
#(1) volume forecasting for SUSQUEDA and
#(2) probabilistic plots:
#Created by D. Mercado-Bettín

library(lubridate); library(imputeTS);library(zoo);library(dplyr)

dir <- "/home/rmarce/volume_ter/"
setwd(dir)

year_initial <- 2024
month_initial <- "10"
members <- 51
fix_plot <- FALSE #to set as default plots and csv outputs
plot_actual <- TRUE #plot current season

out_option <- 3 # 1: median last x days 2: same as last similar season 3: from Comissio de desembassament
min_vol <- 0 #minimum volume in percentage
max_vol <- 1

#initialisation forecast
date_ini <- as.Date(paste0(year_initial,"-",month_initial,"-01"))
all_dates <- seq(from=date_ini, by=1, len=215)
#previous year forecast
date_ini_previous <- as.Date(paste0(year(all_dates[1])-1, "-",month(all_dates[1]), "-01"))
dates_previous <- seq(from=date_ini_previous, by=1, len=215)

#calculated balances
sqd_balance <- read.csv("out/calculated_sqd.csv")

#current balance for the actual dates
sqd_balance_actual <- read.csv(paste0("in/water_balance_sqd.csv"))

#outflow from sau = inflow for sqd
inflow_for_sqd<- read.csv(paste0("out/forecast_sau/for_Qout_sau_",year_initial,"_",month_initial,".csv"))

#median outflows of the last x days
if (out_option==1){
  x_days <- 10
  dates_median_out <- seq(date_ini-x_days,date_ini-1, by=1)
  sqd_balance$date <- as.Date(sqd_balance$date)
  out_median_sqd <- median(sqd_balance[sqd_balance$date %in% dates_median_out,]$Qout)
  print(paste0("SQD median outflow of last ", x_days, ": ",round(out_median_sqd,2)))
  outflow <- rep(out_median_sqd, nrow(inflow_for_sqd))
}

if (out_option==2){
  sqd_balance$date <- as.Date(sqd_balance$date)
  sel_pos <- sqd_balance$date %in% dates_previous
  pseas <- sqd_balance[sel_pos,]
  outflow <- pseas$Qout[1:nrow(inflow_for_sqd)]
}

if (out_option==3){
  outflows_monthly_CD <- read.csv("in/Cabals_outflow_mensuals.csv",stringsAsFactors = FALSE)
  outflows_monthly_CD$date_M_D_Y <- mdy(outflows_monthly_CD$date_M_D_Y)
  outflows_monthly_CD$month_year <- format(outflows_monthly_CD$date, "%Y-%m")
  monthly_values <- outflows_monthly_CD %>%
    group_by(month_year) %>%
    summarise(month_value = first(outflow), .groups = 'drop') 
  
  dates_out <- seq(date_ini,date_ini+nrow(inflow_for_sqd)-1, by=1)
  dates_out_monthly <- format(dates_out, "%Y-%m")
  
  outflow <- data.frame(date = dates_out) %>%
    mutate(month_year = format(date, "%Y-%m")) %>%
    left_join(monthly_values, by = "month_year") %>%
    arrange(date) 
  outflow <- as.data.frame(outflow$month_value)
  }

#Initial volume calculation sqd
V_ini_sqd <- sqd_balance[which(sqd_balance$date==(date_ini-1)),"V"]
change_Q_sqd <- data.frame(matrix(NA,nrow(inflow_for_sqd),members)); 
change_V_sqd <- data.frame(matrix(NA,nrow(inflow_for_sqd),members)); 
V_total_sqd <- data.frame(matrix(NA,nrow(inflow_for_sqd),members))
for (m in 1:members){
  # Qin - Qout
  change_Q_sqd[,m] <- inflow_for_sqd[,(1+m)] - outflow
  
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
  Qout_sqd[,m] <- outflow
  change_Q_sqd[,m] <- inflow_for_sqd[,(1+m)] - Qout_sqd[,m]
  
  #daily volume change per day in hm3
  change_V_sqd[,m] <- change_Q_sqd[,m]*(86400/1e6)
  
  #daily total volume
  V_total_temp <- (V_ini_sqd+change_V_sqd[1,m])
  #assume Qin=Qout when volumen is lower than 5% or greater than 95%
  if (V_total_temp<(unique(sqd_balance$Vmax)*min_vol)){
    Qout_sqd[1,m] <- inflow_for_sqd[1,(1+m)]
    change_Q_sqd[1,m] <- inflow_for_sqd[1,(1+m)] - Qout_sqd[1,m] #should be 0
    change_V_sqd[1,m] <- change_Q_sqd[1,m]*(86400/1e6)
    V_total_temp <- (V_ini_sqd+change_V_sqd[1,m])
  }
  if (V_total_temp>(unique(sqd_balance$Vmax)*max_vol)){
    Qout_sqd[1,m] <- inflow_for_sqd[1,(1+m)]
    change_Q_sqd[1,m] <- inflow_for_sqd[1,(1+m)] - Qout_sqd[1,m] #should be 0
    change_V_sqd[1,m] <- change_Q_sqd[1,m]*(86400/1e6)
    V_total_temp <- (V_ini_sqd+change_V_sqd[1,m])
  }
  V_total_temp_vector <- V_total_temp
  for (d in 2:nrow(inflow_for_sqd)){
    V_total_temp <- (V_total_temp+change_V_sqd[d,m])
    #assume Qin=Qout when volumen is lower than 5% or greater than 95%
    if (V_total_temp<(unique(sqd_balance$Vmax)*min_vol)){
      V_total_temp <- (V_total_temp-change_V_sqd[d,m])
      print(paste0("d: ", d, "m: ", m))
      Qout_sqd[d,m] <- inflow_for_sqd[d,(1+m)]
      print(paste0("Qin: ", inflow_for_sqd[d,(1+m)]))
      print(paste0("Qout: ", Qout_sqd[d,m]))
      change_Q_sqd[d,m] <- inflow_for_sqd[d,(1+m)] - Qout_sqd[d,m] #should be 0
      print(change_Q_sqd[d,m])
      change_V_sqd[d,m] <- change_Q_sqd[d,m]*(86400/1e6)
    }
    if (V_total_temp>(unique(sqd_balance$Vmax)*max_vol)){
      V_total_temp <- (V_total_temp-change_V_sqd[d,m])
      Qout_sqd[1,m] <- inflow_for_sqd[d,(1+m)]
      change_Q_sqd[1,m] <- inflow_for_sqd[d,(1+m)] - Qout_sqd[d,m] #should be 0
      change_V_sqd[1,m] <- change_Q_sqd[d,m]*(86400/1e6)
    }
    V_total_temp_vector <- c(V_total_temp_vector, V_total_temp)
  }
  V_total_sqd[,m] <- V_total_temp_vector
  
}

pdf(paste0("plot/3_forecast_sqd_",year_initial,"_",month_initial,".pdf"))
#plot corrected volumes
plot(as.Date(inflow_for_sqd$date), V_total_sqd[,1], type="l", 
     ylim=c(0,unique(sqd_balance$Vmax)),  ylab="Volume SQD (hm³)", xlab="Date",col="lightgrey")
for (i in 2:51){
  lines(as.Date(inflow_for_sqd$date), V_total_sqd[,i],col="lightgrey")
}
#ensemble mean
#lines(as.Date(inflow_for_sqd$date), rowMeans(V_total_sqd), col="red", lwd=3)
#ensemble meadian!!!RAFA
lines(as.Date(inflow_for_sqd$date), apply(V_total_sqd, 1, median), col="red", lwd=3)
#ensemble percentiles!!!RAFA
lines(as.Date(inflow_for_sqd$date), apply(V_total_sqd, 1, quantile,0.90), lty=2, col="red", lwd=3)
lines(as.Date(inflow_for_sqd$date), apply(V_total_sqd, 1, quantile,0.10), lty=2, col="red", lwd=3)
#real dynamic previous year
dates_plot <- dates_previous
sel_pos <- sqd_balance$date %in% dates_plot
#plot(sqd_balance$date[sel_pos],sqd_balance$V[sel_pos], type="l")
lines(as.Date(inflow_for_sqd$date),sqd_balance$V[sel_pos][1:length(inflow_for_sqd$date)], 
      col="blue", lwd=3)
lines(as.Date(sqd_balance_actual$date, format = "%m/%d/%Y"),sqd_balance_actual$V, 
      col="green", lwd=3)
legend("topleft", legend=c("Ensemble", "Previous season", "51 members", "Actual season"), 
       col=c("red","blue", "black","green"), lty=1, cex=0.8, bty="n")
dev.off()

png(paste0("plot/3_forecast_sqd_",year_initial,"_",month_initial,".png"), width = 800, height = 600, units = "px")
#plot corrected volumes
plot(as.Date(inflow_for_sqd$date), V_total_sqd[,1], type="l", 
     ylim=c(0,unique(sqd_balance$Vmax)),  ylab="Volume SQD (hm³)", xlab="Date",col="lightgrey")
for (i in 2:51){
  lines(as.Date(inflow_for_sqd$date), V_total_sqd[,i],col="lightgrey")
}
#ensemble mean
#lines(as.Date(inflow_for_sqd$date), rowMeans(V_total_sqd), col="red", lwd=3)
#ensemble meadian!!!RAFA
lines(as.Date(inflow_for_sqd$date), apply(V_total_sqd, 1, median), col="red", lwd=3)
#ensemble percentiles!!!RAFA
lines(as.Date(inflow_for_sqd$date), apply(V_total_sqd, 1, quantile,0.90), lty=2, col="red", lwd=3)
lines(as.Date(inflow_for_sqd$date), apply(V_total_sqd, 1, quantile,0.10), lty=2, col="red", lwd=3)
#real dynamic previous year
dates_plot <- dates_previous
sel_pos <- sqd_balance$date %in% dates_plot
#plot(sqd_balance$date[sel_pos],sqd_balance$V[sel_pos], type="l")
lines(as.Date(inflow_for_sqd$date),sqd_balance$V[sel_pos][1:length(inflow_for_sqd$date)], 
      col="blue", lwd=3)
lines(as.Date(sqd_balance_actual$date, format = "%m/%d/%Y"),sqd_balance_actual$V, 
      col="green", lwd=3)
legend("topleft", legend=c("Ensemble", "Previous season", "51 members", "Actual season"), 
       col=c("red","blue", "black","green"), lty=1, cex=0.8, bty="n")
dev.off()

#save results forecast
write.csv(data.frame(date=inflow_for_sqd$date,change_Q_sqd), 
          file=paste0("out/forecast_sqd/for_change_Q_sqd_",year_initial,"_",month_initial,".csv"), 
          quote = F,row.names = F)
write.csv(data.frame(date=inflow_for_sqd$date,change_V_sqd), 
          file=paste0("out/forecast_sqd/for_change_V_sqd_",year_initial,"_",month_initial,".csv"),
          quote = F,row.names = F)
write.csv(data.frame(date=inflow_for_sqd$date,Qout_sqd), 
          file=paste0("out/forecast_sqd/for_Qout_sqd_",year_initial,"_",month_initial,".csv"),
          quote = F,row.names = F)
write.csv(data.frame(date=inflow_for_sqd$date, V_total_sqd),
          file=paste0("out/forecast_sqd/for_V_sqd_",year_initial,"_",month_initial,".csv"),
          quote = F,row.names = F)

if (fix_plot){
  pdf("plot/3_forecast_sqd.pdf")
  #plot corrected volumes
  plot(as.Date(inflow_for_sqd$date), V_total_sqd[,1], type="l", 
       ylim=c(0,unique(sqd_balance$Vmax)),  ylab="Volume SQD (hm³)", xlab="Date",col="lightgrey")
  for (i in 2:51){
    lines(as.Date(inflow_for_sqd$date), V_total_sqd[,i],col="lightgrey")
  }
  #ensemble mean
  #lines(as.Date(inflow_for_sqd$date), rowMeans(V_total_sqd), col="red", lwd=3)
  #ensemble meadian!!!RAFA
  lines(as.Date(inflow_for_sqd$date), apply(V_total_sqd, 1, median), col="red", lwd=3)
  #ensemble percentiles!!!RAFA
  lines(as.Date(inflow_for_sqd$date), apply(V_total_sqd, 1, quantile,0.90), lty=2, col="red", lwd=3)
  lines(as.Date(inflow_for_sqd$date), apply(V_total_sqd, 1, quantile,0.10), lty=2, col="red", lwd=3)
  #real dynamic previous year
  dates_plot <- dates_previous
  sel_pos <- sqd_balance$date %in% dates_plot
  #plot(sqd_balance$date[sel_pos],sqd_balance$V[sel_pos], type="l")
  lines(as.Date(inflow_for_sqd$date),sqd_balance$V[sel_pos][1:length(inflow_for_sqd$date)], 
        col="blue", lwd=3)
  lines(as.Date(sqd_balance_actual$date, format = "%m/%d/%Y"),sqd_balance_actual$V, 
        col="green", lwd=3)
  legend("topleft", legend=c("Ensemble", "Previous season", "51 members", "Actual season"), 
         col=c("red","blue", "black","green"), lty=1, cex=0.8, bty="n")
  dev.off()
  
  png("plot/3_forecast_sqd.png", width = 800, height = 600, units = "px")
  #plot corrected volumes
  plot(as.Date(inflow_for_sqd$date), V_total_sqd[,1], type="l", 
       ylim=c(0,unique(sqd_balance$Vmax)),  ylab="Volume SQD (hm³)", xlab="Date",col="lightgrey")
  for (i in 2:51){
    lines(as.Date(inflow_for_sqd$date), V_total_sqd[,i],col="lightgrey")
  }
  #ensemble mean
  #lines(as.Date(inflow_for_sqd$date), rowMeans(V_total_sqd), col="red", lwd=3)
  #ensemble meadian!!!RAFA
  lines(as.Date(inflow_for_sqd$date), apply(V_total_sqd, 1, median), col="red", lwd=3)
  #ensemble percentiles!!!RAFA
  lines(as.Date(inflow_for_sqd$date), apply(V_total_sqd, 1, quantile,0.90), lty=2, col="red", lwd=3)
  lines(as.Date(inflow_for_sqd$date), apply(V_total_sqd, 1, quantile,0.10), lty=2, col="red", lwd=3)
  #real dynamic previous year
  dates_plot <- dates_previous
  sel_pos <- sqd_balance$date %in% dates_plot
  #plot(sqd_balance$date[sel_pos],sqd_balance$V[sel_pos], type="l")
  lines(as.Date(inflow_for_sqd$date),sqd_balance$V[sel_pos][1:length(inflow_for_sqd$date)], 
        col="blue", lwd=3)
  lines(as.Date(sqd_balance_actual$date, format = "%m/%d/%Y"),sqd_balance_actual$V, 
        col="green", lwd=3)
  legend("topleft", legend=c("Ensemble", "Previous season", "51 members", "Actual season"), 
         col=c("red","blue", "black","green"), lty=1, cex=0.8, bty="n")
  dev.off()
  
  #save results forecast
  write.csv(data.frame(date=inflow_for_sqd$date,change_Q_sqd), 
            file="out/forecast_sqd/for_change_Q_sqd.csv", 
            quote = F,row.names = F)
  write.csv(data.frame(date=inflow_for_sqd$date,change_V_sqd), 
            file="out/forecast_sqd/for_change_V_sqd.csv",
            quote = F,row.names = F)
  write.csv(data.frame(date=inflow_for_sqd$date,Qout_sqd), 
            file="out/forecast_sqd/for_Qout_sqd.csv",
            quote = F,row.names = F)
  write.csv(data.frame(date=inflow_for_sqd$date, V_total_sqd),
            file="out/forecast_sqd/for_V_sqd.csv",
            quote = F,row.names = F)
  
}
