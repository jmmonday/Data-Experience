library(reader)
library(dplyr)
library(readxl)
library(ggplot2)
rm(list=ls())


mice_dataf <- read.csv("C:\\Users\\jonmm\\Desktop\\Neuro Job Summer McGehee\\Female Mice Data Summer 2024.csv")
id_vector <- c(1,2,3,4,5,6,7,8,9,10)

# Create dataframe list
df_list <- list()

for (i in 1:length(id_vector)) {
  dataname <- paste("CData", id_vector[i], sep = "")
  current_df <- data.frame()
  
  # Formatting for ids with two digits
  if (id_vector[i] >= 10) {
    space <- "" 
  } else { space <- 0
  }

  # 2nd For loop
  for (j in 1:28){
    
    # Part to ensure file names work out
    zero <- 0
    if (j >= 10) {
      zero <- ""
    }

    # File path 
    datalocation <- paste("C:\\Users\\jonmm\\Desktop\\Neuro Job Summer McGehee\\FEC2BCSummer2024\\SIP0", space, id_vector[i], "_07", zero, j, "24_00.CSV", sep = "")

    # Check if file exists, if not then skip to the next file
    if (!file.exists(datalocation)){
      next
    }
    
    # Check sides
    if (mice_dataf[j, "Side.Nic"] == "Left") {
      Nicside <- "Left"
      Conside <- "Right"
    }
    else { Nicside <- "Right"
           Conside <- "Left"
    }
    
    # define path
    data <- read.csv(datalocation)
    
    # Set date of collection
    current_df[j, "Date"] <- mice_dataf[j, "Date"]
    
    # Set Duration
    duration_column_name <- paste(Nicside, "Duration", sep = "")
    current_df[j, "NicDuration"] <- tail(data[duration_column_name], n = 1)
    duration_column_name <- paste(Conside, "Duration", sep = "")
    current_df[j, "ConDuration"] <- tail(data[duration_column_name], n = 1)
    
    # Set Licks
    lick_column_name <- paste(Nicside, "Count", sep = "")
    current_df[j, "NicCount"] <- tail(data[lick_column_name], n = 1)
    lick_column_name <- paste(Conside, "Count", sep = "")
    current_df[j, "ConCount"] <- tail(data[lick_column_name], n = 1)
    
    # Set Volume consumed
    nic_column <- paste("M", id_vector[i], ".", "Nic", sep = "")
    current_df[j, "NicVolume"] <- mice_dataf[j, nic_column]
    
    # Set nicotine concentration
    current_df[j, "NicConcentration"] <- mice_dataf[j, "Concentration.Nicotine"]

    # Set Mouse ID
    current_df[j, "ID"] <- id_vector[i]
    
    # Part to create data frames outside the data frame list
    df_list[[dataname]] <- current_df
    
    # Ensure columns are of right data type
    df_list[[i]]$NicDuration <- as.numeric(df_list[[i]]$NicDuration)
    df_list[[i]]$NicCount <- as.numeric(df_list[[i]]$NicCount)
    df_list[[i]]$ConDuration <- as.numeric(df_list[[i]]$ConDuration)
    df_list[[i]]$ConCount <- as.numeric(df_list[[i]]$ConCount)
    
    # Remove high values and N/A values
    #df_list[[i]]$NicDuration[df_list[[i]]$NicDuration > 500] <- NA
    
    dataframename <- dataname
    assign(dataframename, df_list[[i]])
  }
}

# Plot Individual Mice
for (i in 1:length(id_vector)) {
  plot(df_list[[i]]$NicDuration, df_list[[i]]$NicVolume, xlab = "Duration Drinking Nicotine Water (s)", ylab = "Volume Nicotine Consumed (mL)", main = paste("Mouse",id_vector[i], sep = " "), col= ifelse(df_list[[i]]$NicConcentration == 0.00, "red", ifelse(df_list[[i]]$NicConcentration == 0.25,"blue", ifelse(df_list[[i]]$NicConcentration == 0.5, "yellow", ifelse(df_list[[i]]$NicConcentration == 0.75, "green", ifelse(df_list[[i]]$NicConcentration == 1.00, "pink", "purple"))))))
  abline(lm(df_list[[i]]$NicVolume ~ df_list[[i]]$NicDuration))
}


# Fix CData 8
df_list[[8]] <- df_list[[8]] %>%
  add_row(Date = "7/26/2024") %>%
  add_row(Date = "7/27/2024") %>%
  add_row(Date = "7/28/2024")

# Create total data table
CData_Totalf <- rbind(df_list[[1]], df_list[[2]], df_list[[3]], df_list[[4]], df_list[[5]], df_list[[6]], df_list[[7]], df_list[[8]], df_list[[9]], df_list[[10]])
for (i in 1:length(CData_Totalf$Date)) {
  if (is.na(CData_Totalf$Date[i])) {
    replace <- gsub("7/", "", CData_Totalf$Date[i-1])
    replace2 <- gsub("/2024", "", replace)
    replacement <- as.numeric(replace2) + 1
    newdate <- paste("7/", replacement, "/2024", sep = "")
    CData_Totalf$Date[i] <- newdate
  }
}


write.csv(CData_Totalf, "C:\\Users\\jonmm\\Desktop\\Neuro Job Summer McGehee\\Total_female_data_2024summer.csv")

# Make date vector
dates <- c()
for (i in 1:28) {
  dates[i] <- paste("7/", i, "/2024", sep = "")
}

# Make data table for Nicotine duration by date
date_columns <- list()
for (i in 1:28) {
  date <- paste("7/", i, "/2024", sep = "")
  filtered_data <- CData_Totalf %>%
    filter(CData_Totalf$Date == CData_Totalf$Date[i]) %>%
    pull(NicDuration)
  date_columns[[date]] = filtered_data 
}
date_tablef <- as.data.frame(date_columns, stringsAsFactors = FALSE)
names(date_tablef) <- names(dates)

names(date_tablef)

# Make mean vector
meansfdate <- c()
for (i in 1:28) {
  meansfdate[i] <- mean(date_tablef[[i]], na.rm = TRUE)
}

# Combine date and mean vector
mean_date_tablef <- data.frame("Date" = dates, "Means" = meansfdate)

barplot(meansfdate, names.arg = dates, ylab = "Duration per period (s)", main = "Female Nic Duration")

ggplot(CData_Totalf, aes(x = Date, y = NicDuration)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_point(aes(color = NicConcentration), size = 3, position = position_jitter(width = 0.2, height = 0)) +
  theme_minimal() +
  labs(title = "Bar Graph with Individual Data Points",
       x = "Category",
       y = "Value") +
  scale_fill_manual(values = c("A" = "skyblue", "B" = "orange", "C" = "lightgreen"))


