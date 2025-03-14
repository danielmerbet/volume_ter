
#dir <- "~/Documents/intoDBP/volume_ter/"
dir <- "/home/rmarce/volume_ter/"
setwd(dir)

#####MANDATORY FIELDS TO CHANGE###############
year_initial <- 2024
month_initial <- "04" #"01":january, "03":march, "11":november etc...
#outflow option: 1 or 2 or 3
# 1 is the median value of the last x days
# 2 the same as previous season (IT COULD HAVE ERRORS if it was too different than current season)
# 3 outflows from approved volumes in the report of the Comissio de desembassament 
out_option <- 3
#if (out_option==1){ #RAFA: to control this in a different way at Sau and Susqueda
  x_days <- 10
#}
################OPTIONAL######################
plot_actual <- TRUE #plot current season (if there is data available)
#Recommend NOT to change if you do not want to change the default plots in GitHUB README.md
fix_plot <- FALSE #to set as default plots and csv outputs - RAFA: I do not see this implemented, would do things twice
#TRUE only for 2024-10 initialization
bias_corrected <- FALSE #whether to use bias corrected data or not 
##############################################

#Run 1_calculated_balance.R
#the input data must be update first to be able to run:
#in/water_balance_sau.csv
#in/water_balance_sqd.csv
code1 <- "1_calculated_balance.R"
system(paste0("Rscript ", dir,code1))

#Modify and run 2_river.R
code2 <- "2_river.R"
file_read <- readLines(code2)
file_read[8]  <- paste0("dir <- \"", dir, "\"")
file_read[10] <- paste0("year_initial <- ", year_initial)
file_read[11] <- paste0("month_initial <- \"",month_initial,"\"")
file_read[14] <- paste0("fix_plot <- ", fix_plot, " #to set as default plots and csv outputs")
file_read[15] <- paste0("bias_corrected <- ", bias_corrected," #whether to use bias corrected data or not (only for 2024-10 intialization")
writeLines(file_read, con=code2)
system(paste0("Rscript ", dir,code2))

#Modify and run 3_forecast_balance_sau.R
code3 <- "3_forecast_balance_sau.R"
file_read <- readLines(code3)
file_read[8]  <- paste0("dir <- \"", dir, "\"")
file_read[11] <- paste0("year_initial <- ", year_initial)
file_read[12] <- paste0("month_initial <- \"",month_initial,"\"")
file_read[14] <- paste0("fix_plot <- ", fix_plot, " #to set as default plots and csv outputs")
file_read[15] <- paste0("plot_actual <- ", plot_actual," #plot current season")
#For Sau we will make flow option always 1 for the moment
#file_read[17] <- paste0("out_option <- ", out_option," # 1: median last x days 2: same as last similar season")
#if (out_option==1){
  file_read[39] <- paste0("  x_days <- ", x_days) #So we can control this without having outflow option 1 for Susqueda below
 #}
writeLines(file_read, con=code3)
system(paste0("Rscript ", dir,code3))

#Modify and run 3_forecast_balance_sqd.R
code4 <- "3_forecast_balance_sqd.R"
file_read <- readLines(code4)
file_read[8]  <- paste0("dir <- \"", dir, "\"")
file_read[11] <- paste0("year_initial <- ", year_initial)
file_read[12] <- paste0("month_initial <- \"",month_initial,"\"")
file_read[14] <- paste0("fix_plot <- ", fix_plot, " #to set as default plots and csv outputs")
file_read[15] <- paste0("plot_actual <- ", plot_actual," #plot current season")
file_read[17] <- paste0("out_option <- ", out_option," # 1: median last x days 2: same as last similar season 3: from Comissio de desembassament")
if (out_option==1){
  file_read[39] <- paste0("  x_days <- ", x_days)
}
writeLines(file_read, con=code4)
system(paste0("Rscript ", dir,code4))

#Modify and run 3_forecast_balance_ter.R
code5 <- "3_forecast_balance_ter.R"
file_read <- readLines(code5)
file_read[8]  <- paste0("dir <- \"", dir, "\"")
file_read[11] <- paste0("year_initial <- ", year_initial)
file_read[12] <- paste0("month_initial <- \"",month_initial,"\"")
file_read[14] <- paste0("fix_plot <- ", fix_plot, " #to set as default plots and csv outputs")
file_read[15] <- paste0("plot_actual <- ", plot_actual," #plot current season")
writeLines(file_read, con=code5)
system(paste0("Rscript ", dir,code5))

