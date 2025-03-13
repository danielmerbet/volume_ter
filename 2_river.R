#This code provides two outcomes:
#the (1) river forecasting modeling and
#(2) probabilistic plots:
#Created by D. Mercado-Bettín

library(lubridate); library(airGR);

dir <- "~/Documents/intoDBP/volume_ter"
setwd(dir)
year_initial <- 2024
month_initial <- "04"
date_ini <- as.Date(paste0(year_initial,"-",month_initial,"-01"))
all_dates <- seq(from=date_ini, by=1, len=215)
fix_plot <- FALSE#to set as default plots and csv outputs
bias_corrected <- FALSE#whether to use bias corrected data or not (only for 2024-10 intialization

years_fore <- unique(year(all_dates)) #2024:2025 #For System5 it could be a vector of two years, or one single year
years_rean <- (years_fore[1]-2):years_fore[1]# c(2022:2024) #For ERA5 (2 years spin up)
months <- unique(month(all_dates)) #c(10:12,1:4)
last_day <- day(all_dates[length(all_dates)]) #last day of the last month of the forecast, for instance for June is 30 or July is 31

members<-51
area <- 1802660000.000 #m²
date_ini <- as.Date(paste0(years_fore[1], "-", months[1], "-01"))
if (length(years_rean)>1){
  if(length(years_fore)>1){
    date_end <- as.Date(paste0(years_fore[2], "-", months[length(months)], "-", last_day))
  }else{
    date_end <- as.Date(paste0(years_fore[1], "-", months[length(months)], "-", last_day))
  }
  
}else{
  date_end <- as.Date(paste0(years_fore[1], "-", months[length(months)], "-", last_day))
}
spinup <- 2
date_ini_spinup <- as.Date(paste0(years_fore[1]-spinup, "-", months[1], "-01"))

dates_total <- seq(date_ini_spinup, date_end, by=1)
dates_total_forecast <- seq(date_ini, date_end, by=1)
dates_total_reanalysis <- seq(date_ini_spinup, date_ini-1, by=1)

##LOAD DATA
#load forecast
if(bias_corrected){
  load("in/forecast.RData")
}else{
  forecast_list <- list(tp=read.csv(paste0("in/tp_", month_initial,"_",year_initial,".csv")),
                        pev=read.csv(paste0("in/pet_", month_initial,"_",year_initial,".csv")))
  
}

#load reanalysis
load("in/reanalysis_actual_int.RData")
#load calibration parameter
load("in/parameter_calibration.RData")

forecast_data <- list(tp=forecast_list$tp, pev=forecast_list$pev)
reanalysis_data <- list(tp=reanalysis_int$tp, pev=reanalysis_int$pev)

forecast_dates <- seq(from=as.Date(paste0(year_initial,"-",month_initial,"-01")), by=1, length.out=215)

pos_forecast <- forecast_dates %in% dates_total_forecast
pos_reanalysis <- as.Date(reanalysis_data$tp$Dates$start) %in% dates_total_reanalysis

discharge_data <- matrix(, 51, length(dates_total_forecast))
for (member in 1:members){
   
    meteo <- data.frame(dates=dates_total,
                        P=c(reanalysis_data$tp$Data[pos_reanalysis], forecast_list$tp[pos_forecast,member]),
                        E=c(reanalysis_data$pev$Data[pos_reanalysis], forecast_list$pev[pos_forecast,member]))
    meteo$P[meteo$P<0] <- 0
    meteo$E[meteo$E<0] <- 0
    
    #run hydrologic model
    InputsModel <- CreateInputsModel(FUN_MOD = RunModel_GR4J, DatesR = as.POSIXct(meteo$dates),
                                     Precip = meteo$P, PotEvap = meteo$E)
    WarmUp_ini <- as.Date(InputsModel$DatesR[1])
    WarmUp_end <- date_ini-1
    Run_ini <- WarmUp_end+1
    Run_end <- meteo$dates[length(meteo$dates)]
    IndPeriod_WarmUp <- seq(which(format(InputsModel$DatesR, format = "%Y-%m-%d") == WarmUp_ini), 
                            which(format(InputsModel$DatesR, format = "%Y-%m-%d") == WarmUp_end))
    Ind_Run <- seq(which(format(InputsModel$DatesR, format = "%Y-%m-%d") == Run_ini), 
                   which(format(InputsModel$DatesR, format = "%Y-%m-%d") == Run_end))
    RunOptions <- CreateRunOptions(FUN_MOD = RunModel_GR4J,
                                   InputsModel = InputsModel, IndPeriod_Run = Ind_Run,
                                   IndPeriod_WarmUp = IndPeriod_WarmUp)
    #Simulation run with reanalysis time range
    OutputsModel <- RunModel_GR4J(InputsModel = InputsModel, RunOptions = RunOptions, Param = Param)
    #pos_ini <- which(as.Date(OutputsModel$DatesR)==initial_reanalysis_date)
    #pos_end <- length(OutputsModel$DatesR)
    daily_discharge <- OutputsModel$Qsim*area/(1000*86400)
    
    #discharge_out <- c(discharge_out, daily_discharge)
    
  #}
  discharge_data[member,] <- daily_discharge
  print(paste("finished member:",member))
}

#attr(discharge_data, "dimensions") <- c("member", "time")

#save data just in case, before converting to netcdf
#save(discharge_data, file=paste0(getwd(), "/out/discharge_forecast_sau.RData"))
save_data <- data.frame(date=dates_total_forecast)
save_data <- cbind(save_data, as.data.frame(t(data.frame(discharge_data))))

write.csv(save_data, 
          file=paste0(getwd(), "/out/inflow_for_sau_",year_initial,"_",month_initial,".csv"), 
          quote = F, row.names = F)

pdf(paste0("plot/2_inflow_sau_ensemble_",year_initial,"_",month_initial,".pdf"))
plot(save_data$date, discharge_data[1,], type="l", ylim=c(0,700), col="blue")
for (i in 2:51){
  lines(save_data$date,discharge_data[i,], col="blue", ylab="Qin (m³/s)", xlab="Date")
}
dev.off()

png(paste0("plot/2_inflow_sau_ensemble_",year_initial,"_",month_initial,".png"), width = 800, height = 600, units = "px")
plot(save_data$date, discharge_data[1,], type="l", ylim=c(0,700), col="blue")
for (i in 2:51){
  lines(save_data$date, discharge_data[i,], col="blue", ylab="Qin (m³/s)", xlab="Date")
}
dev.off()

if (fix_plot){
  write.csv(save_data, 
            file=paste0(getwd(), "/out/inflow_for_sau.csv"), 
            quote = F, row.names = F)
  
  pdf("plot/2_inflow_sau_ensemble.pdf")
  plot(discharge_data[1,], type="l", ylim=c(0,700), col="blue")
  for (i in 2:51){
    lines(save_data$date, discharge_data[i,], col="blue", ylab="Qin (m³/s)", xlab="Date")
  }
  dev.off()
  
  png("plot/2_inflow_sau_ensemble.png", width = 800, height = 600, units = "px")
  plot(discharge_data[1,], type="l", ylim=c(0,700), col="blue")
  for (i in 2:51){
    lines(save_data$date, discharge_data[i,], col="blue", ylab="Qin (m³/s)", xlab="Date")
  }
  dev.off()
}

