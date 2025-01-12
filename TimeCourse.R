
setwd("C:\\Users\\jonmm\\Desktop")
rm(list=ls())

# Get libraries
library(tidyverse)
library(dplyr)
library(tibble)
library(reader)
library(readxl)
library(data.table)
library(lubridate)


mice_data <- read_csv("C:\\Users\\jonmm\\Desktop\\Neuro Job Summer McGehee\\Female Mice Data Summer 2024.csv", col_names = TRUE)
id_vector <- c(1,2,3,4,5,6,7,8,9,10)
tc_list <- list()

for (i in 1:length(id_vector)) {
  
for (j in 1:length(mice_data$Date)) {

  # Part to ensure file names work out
  zero <- 0
  if (j >= 10) {
    zero <- ""
  }
  space <- 0
  if (i >= 10) {
    space <- ""
  }
  
  # Set path from which data will be extracted
  datastart <- paste("C:\\Users\\jonmm\\Desktop\\Neuro Job Summer McGehee\\FEC2BCSummer2024\\SIP0", space, id_vector[i], "_07", zero, j, "24_00.CSV", sep = "")
  
  # Set path to which data will be sent
  datafinish <- paste("C:\\Users\\jonmm\\Desktop\\Neuro Job Summer McGehee\\Model Data\\TS_0", space, id_vector[i], "_07", zero, j, "24_00.CSV", sep = "")
  
  # Check sides
  if (mice_data[j, "Side Nic"] == "Left") {
    Nicside <- "Left"
    Conside <- "Right"
    side <- "Left"
  }
  else { Nicside <- "Right"
  Conside <- "Left"
  side <- "Right"
  }
  
  # Check if file exists, if not then skip to the next file
  if (!file.exists(datastart)){
    next
  }

  # Set variables
  total_volume = mice_data[j, paste(paste("M", id_vector[i], sep = ""), "Nic", sep = " ")]
  NConcentration = mice_data[j, "Concentration Nicotine"]
  
  # Read file and convert into data table
  tdata1 <- read.csv(datastart)
  data2 <- data.frame(matrix(0, nrow = as.numeric(tdata1$ElapsedSecs[nrow(tdata1)]), ncol = 2))
  data2$Timecourse <- 1:tdata1$ElapsedSecs[nrow(tdata1)]
  colnames(data2)[1] = "Time"
  colnames(data2)[2] = "LickDuration"
  
  # For 1 lick bouts, write out the duration it took
  for (k in 2:nrow(tdata1)) {
    if ((tdata1[k, paste(side, "Count", sep = "")] - tdata1[k-1, paste(side, "Count", sep = "")]) >= 1) {
      elapsed_secs <- tdata1[k, "ElapsedSecs"]
      data2[elapsed_secs, "LickDuration"] <- data2[elapsed_secs, "LickDuration"] + (tdata1[k, paste(side, "Duration", sep = "")] - tdata1[k-1, paste(side, "Duration", sep = "")])
    }
  }
  
  # For more than 1 lick bouts, last lick occurs at the recording after blind spot with average duration per lick and 
  # blind spot recording durations are added to the previous bout
  # for (m in 2:nrow(tdata1)) {
    # if ((tdata1[m, paste(side, "Count", sep = "")] - tdata1[m-1, paste(side, "Count", sep = "")]) > 1) {
     #  elapsedlicks <- (tdata1[m, paste(side, "Count", sep = "")] - tdata1[m-1, paste(side, "Count", sep = "")]) 
     #  elapsed_secs <- tdata1[m, "ElapsedSecs"]
     #  elapsed_secs_prior <- tdata1[m-1, "ElapsedSecs"]
     # elapsed_duration <- tdata1[m, paste(side, "Duration", sep = "")] - tdata1[m-1, paste(side, "Duration", sep = "")]
    #  leftoverDuration <- elapsed_duration/elapsedlicks 
      
   #   data2[elapsed_secs_prior, "LickDuration"] <- data2[elapsed_secs_prior, "LickDuration"] + (elapsed_duration - leftoverDuration)
    #  data2[elapsed_secs, "LickDuration"] <- data2[elapsed_secs, "LickDuration"] + leftoverDuration
  #  }
 # }

  # Input times into model data
  for (z in 1:nrow(tdata1)) {
    data2[tdata1[z, "ElapsedSecs"], "Time"] <-  tdata1[z, "MM.DD.YYYY.hh.mm.ss"]
  }
  
  # Create time course by the hour
  tc_df <- data.frame(matrix(0, nrow = 21, ncol = 1))
  colnames(tc_df)[1] <- "Lick Duration"
  for (t in 0:20) {
    tc_df[t+1, "Lick Duration"] <- sum(data2$LickDuration[(t*3600 + 1):((t + 1)*3600)])
  }
  
  # Add the hourly data frame to the list at index i
  tc_list[[paste("ID_", id_vector[i], "_Day_", j, sep = "")]] <- tc_df
  
  # Write data to model data folder
  #write.csv(data2, datafinish)
  
}
}

mp_list <- list()
for (i in 1:length(tc_list)) {
  time_series <- tc_list[[i]]
  mp_result <- tsmp(time_series, window_size = 4, exclusion_zone = 1/2)
  mp_list[[i]] <- mp_result
}
print(mp_list[[77]])
plot(mp_list[[77]]$mp, type = "l", main = "Matrix Profile", 
     xlab = "Index", ylab = "Matrix Profile Value")



