library(lubridate); library(imputeTS); library(zoo)

#SAU
#data since 2017
#load original data: volume, inflows, outflows and gate extractions
dir <- "/home/rmarce/volume_ter/"
setwd(dir)

data <- read.csv("in/water_balance_sau.csv")
data$date <- as.Date(data$date, format = "%m/%d/%Y")

#checking data
summary(data)

#Rafa: I changed this part so the calculation is closer to what they do historically
# It is just a matter of how you consider volumes and date of inflow/outflow
#calculate Qin from V (hm3/d) and Qout (m3/s): Qin[i] = (V[i] -V[i-1]) + Qout[i]; i=time
diffV_calc <- (data$V[2:nrow(data)] - data$V[1:(nrow(data)-1)])*(1e6/86400)
Qin_calc <-  diffV_calc + data$Qout[2:(nrow(data))]
data$Qin_calc <- c(NA,Qin_calc)

#Indentify negative Qin, this values will be set as 0, and this could affect the heat budget
#RAFA: I changed the criterion, we will set negative values as linear interpolation between good values
# and then we will change the rest of the budget accordingly
data$neg_Qin <- NA
data$neg_Qin[data$Qin_calc<0] <- "negative"
#data$Qin_calc[data$Qin_calc<0] <- 0
data$Qin_calc[data$Qin_calc<0] <- NA
Qin_calc_interp <- na.approx(data$Qin_calc)
data$Qin_calc_interp <- c(NA,Qin_calc_interp)

#now we recalculate Qout so the water balance will be right:
#which(data$Qin_calc<0)
#data$Qout_calc <- data$Qout
Qout_calc <- data$Qin_calc_interp[2:(nrow(data))] - diffV_calc
data$Qout_calc <- c(NA,Qout_calc)
positions<-which(data$Qout_calc<0)
#we fix one negative value
data$Qin_calc_interp[positions] <-  data$Qin_calc_interp[positions] + abs(data$Qout_calc[positions])
data$Qout_calc[positions] <- 0

#reassign to respect naming convention by Dani o the outputs can be used by other codes
data$Qin_calc_before_interp <- data$Qin_calc
data$Qin_calc <- data$Qin_calc_interp

#remove first row
data <- data[2:nrow(data),]
#data <- data[1:(nrow(data)-1),] #Rafa: I do not think we need this line now with the new calculation


write.csv(data, file="out/calculated_sau.csv",
          row.names = F, quote = F)

#dev.off()
pdf("plot/1_initial_volumes_sau.pdf")
op<- par(mfrow=c(2,1), mar=c(3,4,1,2)+.1)
plot(data$date, data$Qin_calc, type="l", col="blue", ylab="Qin/Qout (m³/s)", xlab="Date")
lines(data$date, data$Qout_calc, col="red")
legend("topright", legend=c("Qin", "Qout"), col=c("blue", "red"), lty=1, cex=0.8, bty="n")
plot(data$date, data$V, type="l", ylab="Volume (m³)", xlab="Date")
dev.off()

png("plot/1_initial_volumes_sau.png", width = 800, height = 600, units = "px")
op<- par(mfrow=c(2,1), mar=c(3,4,1,2)+.1)
plot(data$date, data$Qin_calc, type="l", col="blue", ylab="Qin/Qout (m³/s)", xlab="Date")
lines(data$date, data$Qout_calc, col="red")
legend("topright", legend=c("Qin", "Qout"), col=c("blue", "red"), lty=1, cex=0.8, bty="n")
plot(data$date, data$V, type="l", ylab="Volume (m³)", xlab="Date")
dev.off()

#SUSQUEDA
#data since 2017
#load original data: volume, inflows, outflows and gate extractions
data <- read.csv("in/water_balance_sqd.csv")
data$date <- as.Date(data$date, format = "%m/%d/%Y")

#checking data
summary(data)

#RAFA: I changed this calculation, see comment above for Sau
#calculate Qin from V (hm3/d) and Qout (m3/s): Qin[i-1] = (V[i] -V[i-1]) + Qout[i-1]; i=time
diffV_calc <- (data$V[2:nrow(data)] - data$V[1:(nrow(data)-1)])*(1e6/86400)
Qin_calc <-  diffV_calc + data$Qout[2:(nrow(data))]
data$Qin_calc <- c(NA,Qin_calc)

#indentify negative Qin, this values will be set as 0, and this could affect the heat budget
#RAFA: I changed the criterion, we will set negative values as linear interpolation between good values
data$neg_Qin <- NA
data$neg_Qin[data$Qin_calc<0] <- "negative"
#data$Qin_calc[data$Qin_calc<0] <- 0
data$Qin_calc[data$Qin_calc<0] <- NA
Qin_calc_interp <- na.approx(data$Qin_calc)
data$Qin_calc_interp <- c(NA,Qin_calc_interp)

#now we recalculate Qout so the water balance will be right:
#which(data$Qin_calc<0)
#data$Qout_calc <- data$Qout
Qout_calc <- data$Qin_calc_interp[2:(nrow(data))] - diffV_calc
data$Qout_calc <- c(NA,Qout_calc)
positions<-which(data$Qout_calc<0)
#we fix one negative value
data$Qin_calc_interp[positions] <-  data$Qin_calc_interp[positions] + abs(data$Qout_calc[positions])
data$Qout_calc[positions] <- 0

#reassign to respect naming convention by Dani
data$Qin_calc_before_interp <- data$Qin_calc
data$Qin_calc <- data$Qin_calc_interp

#remove first row
data <- data[2:nrow(data),]
#data <- data[1:(nrow(data)-1),] #RAFA: I do not think you need this

write.csv(data, file="out/calculated_sqd.csv",
          row.names = F, quote = F)


pdf("plot/1_initial_volumes_sqd.pdf")
op<- par(mfrow=c(2,1), mar=c(3,4,1,2)+.1)
plot(data$date, data$Qin_calc, type="l", col="blue", ylab="Qin/Qout (m³/s)", xlab="Date")
lines(data$date, data$Qout_calc, col="red")
legend("topright", legend=c("Qin", "Qout"), col=c("blue", "red"), lty=1, cex=0.8, bty="n")
plot(data$date, data$V, type="l", ylab="Volume (m³)", xlab="Date")
dev.off()

png("plot/1_initial_volumes_sqd.png", width = 800, height = 600, units = "px")
op<- par(mfrow=c(2,1), mar=c(3,4,1,2)+.1)
plot(data$date, data$Qin_calc, type="l", col="blue", ylab="Qin/Qout (m³/s)", xlab="Date")
lines(data$date, data$Qout_calc, col="red")
legend("topright", legend=c("Qin", "Qout"), col=c("blue", "red"), lty=1, cex=0.8, bty="n")
plot(data$date, data$V, type="l", ylab="Volume (m³)", xlab="Date")
dev.off()


