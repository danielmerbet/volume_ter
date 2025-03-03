dir <- "/home/dmercado/Documents/intoDBP/volume_ter/"
setwd(dir)

#load sau and sqd volumes
V_sau <- read.csv("out/forecast_sau/for_V_sau.csv")
V_sqd <- read.csv("out/forecast_sqd/for_V_sqd.csv")

V_total <- V_sau[2:ncol(V_sau)] + V_sqd[2:ncol(V_sqd)]

#load sau and sqd balances
sqd_balance <- read.csv("out/calculated_sqd.csv")
sau_balance <- read.csv("out/calculated_sau.csv")
ter_balance_V <- sqd_balance$V + sau_balance$V
#current balance for the actual dates
sqd_balance_actual <- read.csv("in/water_balance_sqd_actual.csv")
sau_balance_actual <- read.csv("in/water_balance_sau_actual.csv")
ter_balance_actual_V <- sqd_balance_actual$V+sau_balance_actual$V

pdf("plot/3_forecast_ter.pdf")
#plot corrected volumes
plot(as.Date(V_sau$date), V_total[,1], type="l", 
     ylim=c(0,unique(sqd_balance$Vmax+sau_balance$Vmax)),  ylab="Volume Ter (hm³)", xlab="Date")
for (i in 2:51){
  lines(as.Date(V_sau$date), V_total[,i])
}
#ensemble mean
lines(as.Date(V_sau$date), rowMeans(V_total), col="red", lwd=3)
#real dynamic previous year
dates_plot <- seq(as.Date("2023-10-01"), as.Date("2024-04-30"), by=1)
sel_pos <- as.Date(sqd_balance$date) %in% dates_plot
#plot(sqd_balance$date[sel_pos],sqd_balance$V[sel_pos], type="l")
lines(as.Date(V_sau$date),ter_balance_V[sel_pos][1:length(V_sau$date)], 
      col="blue", lwd=3)
lines(as.Date(sau_balance_actual$date, format = "%m/%d/%Y"),ter_balance_actual_V, 
      col="green", lwd=3)
legend("topleft", legend=c("Ensemble", "Last season", "51 members", "Actual season"), 
       col=c("red","blue", "black","green"), lty=1, cex=0.8, bty="n")
dev.off()

png("plot/3_forecast_ter.png", width = 800, height = 600, units = "px")
#plot corrected volumes
plot(as.Date(V_sau$date), V_total[,1], type="l", 
     ylim=c(0,unique(sqd_balance$Vmax+sau_balance$Vmax)),  ylab="Volume Ter (hm³)", xlab="Date")
for (i in 2:51){
  lines(as.Date(V_sau$date), V_total[,i])
}
#ensemble mean
lines(as.Date(V_sau$date), rowMeans(V_total), col="red", lwd=3)
#real dynamic previous year
dates_plot <- seq(as.Date("2023-10-01"), as.Date("2024-04-30"), by=1)
sel_pos <- as.Date(sqd_balance$date) %in% dates_plot
#plot(sqd_balance$date[sel_pos],sqd_balance$V[sel_pos], type="l")
lines(as.Date(V_sau$date),ter_balance_V[sel_pos][1:length(V_sau$date)], 
      col="blue", lwd=3)
lines(as.Date(sau_balance_actual$date, format = "%m/%d/%Y"),ter_balance_actual_V, 
      col="green", lwd=3)
legend("topleft", legend=c("Ensemble", "Last season", "51 members", "Actual season"), 
       col=c("red","blue", "black","green"), lty=1, cex=0.8, bty="n")
dev.off()
