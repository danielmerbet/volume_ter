#This is just to add observations to the 2025 plot. Not for including in the workflow
pdf("plot/3_forecast_ter_for_2025.pdf",width=8)


par(mar=c(4.1, 4.1, 4.1, 8))
plot(as.Date(V_sau$date), V_total[,1], type="l", 
     ylim=c(0,unique(sqd_balance$Vmax+sau_balance$Vmax)),
     ylab="Volume Sau+Susqueda (hm³)", xlab="", col="lightgrey",
     main=paste0("Predicció inicialitzada el ",date_ini))
for (i in 2:51){
  lines(as.Date(V_sau$date), V_total[,i], col="lightgrey")
}
# Ensemble percentiles
# 0.90 and 0.10 percentiles
upper_90 <- apply(V_total, 1, quantile, 0.90)
lower_10 <- apply(V_total, 1, quantile, 0.1)

# Plot the shaded area between 0.90 and 0.10 percentiles with transparency
polygon(c(as.Date(V_sau$date), rev(as.Date(V_sau$date))), 
        c(upper_90, rev(lower_10)),
        col=rgb(0, 0, 1, 0.2), border=NA)  # Red with transparency

# 0.75 and 0.25 percentiles
upper_75 <- apply(V_total, 1, quantile, 0.75)
lower_25 <- apply(V_total, 1, quantile, 0.25)

# Plot the shaded area between 0.75 and 0.25 percentiles with transparency
polygon(c(as.Date(V_sau$date), rev(as.Date(V_sau$date))), 
        c(upper_75, rev(lower_25)),
        col=rgb(0, 0, 0.6, 0.3), border=NA)  # Dark red with transparency

# Plot the ensemble median
lines(as.Date(V_sau$date), apply(V_total, 1, median), col="blue", lwd=3)


#real dynamic previous year
dates_plot <- dates_previous
sel_pos <- as.Date(sqd_balance$date) %in% dates_plot
#plot(sqd_balance$date[sel_pos],sqd_balance$V[sel_pos], type="l")
lines(as.Date(V_sau$date),ter_balance_V[sel_pos][1:length(V_sau$date)], 
      col="black", lwd=1.5)
lines(as.Date(sau_balance_actual$date, format = "%m/%d/%Y"),ter_balance_actual_V, 
      col="green", lwd=3)

#limits d'alertes
abline(200,0,lty=3)
text(all_dates[215]+7, 200, "Prealerta",xpd=NA,pos = 4,cex=0.8)

outflows_ALERTA <- c(120,140,160,160,160,160,160,140,120,120,120,120)#first in Jan
outflows_ALERTA_daily <- data.frame(date = all_dates) %>%
  mutate(month_alerta = as.numeric(format(date, "%m"))) %>%
  mutate(value = outflows_ALERTA[.data$month_alerta]) %>%
  arrange(date)
lines(outflows_ALERTA_daily$date, outflows_ALERTA_daily$value,col="yellow",lty=3,lwd=2)
text(all_dates[215]+7, outflows_ALERTA_daily$value[length(outflows_ALERTA_daily$value)], "Alerta",
     xpd=NA,pos = 4,cex=0.8)

lines(outflows_ALERTA_daily$date, outflows_ALERTA_daily$value+20,col="yellow",lty=3,lwd=1)
text(all_dates[215]+7, outflows_ALERTA_daily$value[length(outflows_ALERTA_daily$value)]+20, 
     "Sortida Alerta",xpd=NA,pos = 4,cex=0.8)

abline(98,0,lty=3,col="darkorange",lwd=1)
text(all_dates[215]+7, 98, "Sortida Excep.",xpd=NA,pos = 4,cex=0.8)
abline(85,0,lty=3,col="darkorange",lwd=2)
text(all_dates[215]+7, 85, "Excepcionalitat",xpd=NA,pos = 4,cex=0.8)
abline(65,0,lty=3,col="red",lwd=2)
text(all_dates[215]+7, 65, "Emergència I",xpd=NA,pos = 4,cex=0.8)
abline(43,0,lty=3,col="red",lwd=2)
text(all_dates[215]+7, 43, "Emergència II",xpd=NA,pos = 4,cex=0.8)
abline(14,0,lty=3,col="red",lwd=2)
text(all_dates[215]+7, 14, "Emergència III",xpd=NA,pos = 4,cex=0.8)


legend(all_dates[215]+7,unique(sqd_balance$Vmax+sau_balance$Vmax), legend=c("51 members", "Ensemble median",  "Observations" ,"Previous season"), 
       col=c("lightgrey","blue", "green","black"), lty=1, cex=0.8, bty="n",xpd=NA,lwd=1.5)
text(all_dates[215]+7, unique(sqd_balance$Vmax+sau_balance$Vmax)-75, "Area fosca = 50% prob.\nArea clara = 80% prob.",xpd=NA,pos = 4,cex=0.8)

new_obs <- read.csv("in/dades_ACA_marc_2025.csv")
new_obs$date <- mdy(new_obs$date)
lines(new_obs$date, new_obs$volume, col="green", lwd=3)
dev.off()
