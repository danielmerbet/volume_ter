#This code has two main steps: the (1) river forecasting modeling and (2) probabilistic plots:
#Created by D. Mercado-Bettín

library(loadeR);library(lubridate); library(visualizeR); library(sp); library(airGR);

dir <- "/home/dmercado/Documents/intoDBP/volume_ter/"
setwd(dir)
years_fore <- 2024:2025 #For System5 it could be a vector of two years, or one single year
years_rean <- c(2022:2024) #For ERA5 (2 years spin up)
months <- c(10:12,1:4)
last_day <- 30 #last day of the last month of the forecast, for instance for June is 30 or July is 31
area <- 1802660000.000 #m²
date_ini <- as.Date(paste0(years_fore[1], "-", months[1], "-01"))
if (length(years_rean)>1){
  date_end <- as.Date(paste0(years_fore[2], "-", months[length(months)], "-", last_day))
}else{
  date_end <- as.Date(paste0(years_fore[1], "-", months[length(months)], "-", last_day))
}
spinup <- 2
date_ini_spinup <- as.Date(paste0(years_fore[1]-spinup, "-", months[1], "-01"))

dates_total <- seq(date_ini_spinup, date_end, by=1)
dates_total_forecast <- seq(date_ini, date_end, by=1)

members<-51
##LOAD DATA
#load forecast
load("in/forecast.RData")
#load reanalysis
load("in/reanalysis_actual_int.RData")
#load calibration parameter
load("in/parameter_calibration.RData")

forecast_data <- list(tp=forecast_list$tp, pev=forecast_list$pev)
reanalysis_data <- list(tp=reanalysis_int$tp, pev=reanalysis_int$pev)

forecast_dates <- seq(from=as.Date("2024-10-01"), by=1, length.out=215)

pos_forecast <- forecast_dates %in% dates_total_forecast
pos_reanalysis <- as.Date(reanalysis_data$tp$Dates$start) %in% dates_total

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
save_data <- data_frame(date=dates_total_forecast)
save_data <- cbind(save_data, as.data.frame(t(data.frame(discharge_data))))

write.csv(save_data, 
          file=paste0(getwd(), "/out/inflow_for_sau.csv"), quote = F,
          row.names = F)


pdf("plot/2_inflow_sau_ensemble.pdf")
plot(discharge_data[1,], type="l", ylim=c(0,700), col="blue")
for (i in 2:51){
  lines(discharge_data[i,], col="blue", ylab="Qin (m³/s)", xlab="Date")
}
dev.off()

png("plot/2_inflow_sau_ensemble.png", width = 800, height = 600, units = "px")
plot(discharge_data[1,], type="l", ylim=c(0,700), col="blue")
for (i in 2:51){
  lines(discharge_data[i,], col="blue", ylab="Qin (m³/s)", xlab="Date")
}
dev.off()