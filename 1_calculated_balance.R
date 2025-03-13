library(lubridate); library(imputeTS)

#SAU
#data since 2017
#load original data: volume, inflows, outflows and gate extractions
dir <- "~/Documents/intoDBP/volume_ter"
setwd(dir)

data <- read.csv("in/water_balance_sau.csv")
data$date <- as.Date(data$date, format = "%m/%d/%Y")

#checking data
summary(data)

#calculate Qin from V (hm3/d) and Qout (m3/s): Qin[i-1] = (V[i] -V[i-1]) + Qout[i-1]; i=time
diffV_calc <- (data$V[2:nrow(data)] - data$V[1:(nrow(data)-1)])*(1e6/86400)
Qin_calc <-  diffV_calc + data$Qout[1:(nrow(data)-1)]
data$Qin_calc <- c(Qin_calc, NA)

#add Qin_calc to all data
data <- data[2:nrow(data),]
data <- data[1:(nrow(data)-1),]

#some Qin_calc are negative, this means there are errors in Qout? that must be corrected:
which(data$Qin_calc<0)
data$Qout_calc <- data$Qout
data$Qout_calc[data$Qin_calc<0] <- data$Qout[data$Qin_calc<0] - data$Qin_calc[data$Qin_calc<0]

#indentify negative Qin, this values will be set as 0, and this could affect the heat budget
data$neg_Qin <- NA
data$neg_Qin[data$Qin_calc<0] <- "negative"
data$Qin_calc[data$Qin_calc<0] <- 0

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

#calculate Qin from V (hm3/d) and Qout (m3/s): Qin[i-1] = (V[i] -V[i-1]) + Qout[i-1]; i=time
diffV_calc <- (data$V[2:nrow(data)] - data$V[1:(nrow(data)-1)])*(1e6/86400)
Qin_calc <-  diffV_calc + data$Qout[1:(nrow(data)-1)]
data$Qin_calc <- c(Qin_calc, NA)

#add Qin_calc to all data
data <- data[2:nrow(data),]
data <- data[1:(nrow(data)-1),]

#some Qin_calc are negative, this means there are errors in Qout? that must be corrected:
which(data$Qin_calc<0)
data$Qout_calc <- data$Qout
data$Qout_calc[data$Qin_calc<0] <- data$Qout[data$Qin_calc<0] - data$Qin_calc[data$Qin_calc<0]

#indentify negative Qin, this values will be set as 0, and this could affect the heat budget
data$neg_Qin <- NA
data$neg_Qin[data$Qin_calc<0] <- "negative"
data$Qin_calc[data$Qin_calc<0] <- 0

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


