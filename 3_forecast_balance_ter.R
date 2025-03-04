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

#add ACA data
dates_aca <- seq(as.Date("2023-04-01"), as.Date("2025-05-31"),1)
aca_min <- read.csv("in/aca_report_min.csv", header = F)
aca_min$dates <- dates_aca[round(aca_min$V1)]
aca_p5 <- read.csv("in/aca_report_p5.csv", header = F)
aca_p5$dates <- dates_aca[round(aca_p5$V1)]
aca_p15 <- read.csv("in/aca_report_p15.csv", header = F)
aca_p15$dates <- dates_aca[round(aca_p15$V1)]
aca_p25 <- read.csv("in/aca_report_p25.csv", header = F)
aca_p25$dates <- dates_aca[round(aca_p25$V1)]
aca_p50 <- read.csv("in/aca_report_p50.csv", header = F)
aca_p50$dates <- dates_aca[round(aca_p50$V1)]

get_col_transparent <- function(color, alpha) {
  rgb_vals <- col2rgb(color) / 255  # Convert to 0-1 scale
  rgb(rgb_vals[1], rgb_vals[2], rgb_vals[3], alpha = alpha)
}

pdf("plot/3_forecast_ter_ACA.pdf")
#plot corrected volumes
plot(as.Date(V_sau$date), V_total[,1], type="l", col="lightgray",
     ylim=c(0,unique(sqd_balance$Vmax+sau_balance$Vmax)),  ylab="Volume Ter (hm³)", xlab="Date")
for (i in 2:51){
  lines(as.Date(V_sau$date), V_total[,i], col="lightgray")
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
#ACA data
lines(aca_p50$dates, aca_p50$V2, lty=2 ,col="darkblue", lwd=3)
lines(aca_p25$dates, aca_p25$V2, lty=2 ,col="lightblue", lwd=3)
lines(aca_p15$dates, aca_p15$V2, lty=2 ,col="gray", lwd=3)
lines(aca_p5$dates, aca_p5$V2, lty=2 ,col="gold3", lwd=3)
lines(aca_min$dates, aca_min$V2, lty=2 ,col="darkred", lwd=3)
#Emergència III
ini_date <- as.Date(V_sau$date)[1]
end_date <- as.Date(V_sau$date)[dim(V_sau)[1]]
rect(xleft = ini_date-30, ybottom = 0, 
     xright = end_date+30, ytop = 14, 
     col = get_col_transparent("brown3", 0.15), border = NA)
#Emergència II
rect(xleft = ini_date-30, ybottom = 14, 
     xright = end_date+30, ytop = 42.6, 
     col = get_col_transparent("brown1", 0.15), border = NA)
#Emergència I
rect(xleft = ini_date-30, ybottom = 42.6, 
     xright = end_date+30, ytop = 64.60, 
     col = get_col_transparent("coral1", 0.15), border = NA)
#Excepcionalitat
rect(xleft = ini_date-30, ybottom = 64.60, 
     xright = end_date+30, ytop = 85.20, 
     col = get_col_transparent("orange", 0.15), border = NA)
#Alerta
rect(xleft = ini_date-30, ybottom = 85.20, 
     xright = end_date+30, ytop = 121, 
     col = get_col_transparent("yellow", 0.15), border = NA)
rect(xleft = as.Date("2025-02-01"), ybottom = 121, 
     xright = end_date+30, ytop = 140, 
     col = get_col_transparent("yellow", 0.15), border = NA)
rect(xleft = as.Date("2025-03-01"), ybottom = 140, 
     xright = end_date+30, ytop = 160, 
     col = get_col_transparent("yellow", 0.15), border = NA)

legend("topleft", legend=c("Ensemble", "51 members", 
                           "Actual season", "Last season", 
                           "P50", "P25", "P15", "P5", "MIN"), 
       col=c("red","lightgray",
             "green","blue",
             "darkblue","lightblue","gray","gold3","darkred"), 
       lty=c(rep(1,4),rep(2,5)), cex=0.8, bty="n")
dev.off()

