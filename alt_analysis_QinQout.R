library(dplyr)
  
sau_balance <- read.csv("out/calculated_sau.csv")
sau_balance$date <- as.Date(sau_balance$date)

#select previous season
dates_previous <- seq(as.Date("2023-10-01"), as.Date("2024-04-30"), by=1)
sel_pos <- sau_balance$date %in% dates_previous
pseas <- sau_balance[sel_pos,]

op<- par(mfrow=c(2,1), mar=c(3,4,1,2)+.1)
plot(pseas$date, pseas$V, type="l", col="black", lwd=3)
plot(pseas$date, pseas$Qin_calc, col="blue", type="l")
lines(pseas$date, pseas$Qout_calc, col="red")

# Calculate the 5-day moving average (eman)
pseas$Qout5 <- zoo::rollmean(pseas$Qout, k = 5, align = "right", fill = NA)
pseas$Qin5 <- zoo::rollmean(pseas$Qin, k = 5, align = "right", fill = NA)
indices <- seq(5, nrow(pseas), by = 5)  # Indices for every 5th day
lines(pseas$date[indices], pseas$Qout5[indices], col = "violet", pch = 19, cex = 1)
lines(pseas$date[indices], pseas$Qin5[indices], col = "green", pch = 19, cex = 1)

#try using 5-days moving average of the 7 days in advance forecasted
pseas <- pseas %>%
  mutate(Qin_shifted = dplyr::lead(Qin, n = 6),  # Shift Qin forward by 6 days
         Q5d7a = zoo::rollmean(Qin_shifted, k = 5, align = "left", fill = NA)) %>%
  dplyr::select(-Qin_shifted)  # Remove the intermediate shifted column

lines(pseas$date, pseas$Q5d7a, type = "l", col = "black", lty = 2)

plot(pseas$Qin, pseas$Qout)
