# Get libraries
library(tidyverse)
library(dplyr)
library(tibble)
library(reader)
library(readxl)
library(data.table)
library(lubridate)
library(matrixprofiler)
library(openxlsx)
library(ggplot2)
library(matrixProfile)
library(data.table)

rm(list=ls())


big_list <- list()
for (i in length(tc_list)){
  datats <- tc_list[[i]]
  datats <- c(datats$`Lick Duration`)
  datanew <- mass_pre(datats, 3, query = NULL, type = "normalized", weights = NULL)
  datalarge <- mass(datanew, datats, index = 1, version = "v3", n_workers = 1)
  big_list[[i]] <- datalarge
}

datats <- tc_list[[1]]
datats <- c(datats$`Lick Duration`)
datats <- as.numeric(datats)
typeof(datats)
datalarge <- mass(datats[1:5], datats)

mp_result <- stamp(datats, 3, query = NULL, exclusion_zone = 0.5, s_size = 1, n_workers = 1, progress = TRUE)
mp_result <- stamp(datats, 3)

-------------------


library(matrixprofiler)
library(ggplot2)

# Function to compute matrix profile using STAMP
compute_matrix_profile <- function(time_series, window_size) {
  # Check if time_series is numeric
  if (!is.numeric(time_series)) {
    stop("Time series data must be numeric.")
  }
  
  # Check if window_size is a positive integer
  if (!is.numeric(window_size) || window_size <= 0 || window_size %% 1 != 0) {
    stop("Window size must be a positive integer.")
  }
  
  # Check if window_size is less than the length of the time series
  if (window_size >= length(time_series)) {
    stop("Window size must be less than the length of the time series.")
  }
  
  # Compute the matrix profile using STAMP
  mp_result <- tryCatch({
    stamp(time_series, window_size)
  }, error = function(e) {
    stop("Error during matrix profile computation: ", e$message)
  })
  
  # Return the matrix profile and index profile
  list(
    matrix_profile = mp_result$matrix_profile,
    index_profile = mp_result$index_profile
  )
}

# Example time series data (replace with your actual data)
example_time_series <- rnorm(100)  # Random normal data for example
window_length <- 10  # Example window size

# Compute matrix profile
result <- compute_matrix_profile(example_time_series, window_length)

# Print results
print(result$matrix_profile)
print(result$index_profile)

# Optional: Plot matrix profile
matrix_profile_df <- data.frame(
  Index = 1:length(result$matrix_profile),
  MatrixProfile = result$matrix_profile
)

ggplot(matrix_profile_df, aes(x = Index, y = MatrixProfile)) +
  geom_line() +
  labs(title = "Matrix Profile", x = "Index", y = "Matrix Profile Value")


stamp(rnorm(100, 50, 2), 5)





---------------
Duration_data <- list()
mice_data <- read_csv("C:\\Users\\jonmm\\Desktop\\Neuro Job Summer McGehee\\Female Mice Data Summer 2024.csv", col_names = TRUE)
id_vector <- c(1,2,3,4,5,6,7,8,9,10)

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
 
    
    nic_vec <- diff(tdata1[1:length(tdata1$ElapsedSecs), paste(Nicside, "Duration", sep = "")])
    nic_vec <- nic_vec[nic_vec != 0]
    con_vec <- diff(tdata1[1:length(tdata1$ElapsedSecs), paste(Conside, "Duration", sep = "")])
    con_vec <- con_vec[con_vec != 0]
    
    Duration_data[[paste(id_vector[i], "_Nic", "_07", zero, j, sep = "")]] <- nic_vec
    Duration_data[[paste(id_vector[i], "_Con", "_07", zero, j, sep = "")]] <-  con_vec
    
  }
}

# Find the maximum length of the vectors
max_length <- max(sapply(Duration_data, length))

# Extend all vectors to the same length by padding with NA
padded_vec_list <- lapply(Duration_data, function(x) {
  c(x, rep(NA, max_length - length(x)))
})

# Combine the vectors into a data frame
df <- data.frame(padded_vec_list)


# Making each list and vector for duration by concentrations
df_all <- c() 
# Loop through each vector in the list and concatenate
for (vec in Duration_data) {
  df_all <- c(df_all, vec)
}
df_all <- data.frame(df_all[df_all < 100])
colnames(df_all)[1] <- "All_Durations"



df_0.25 <- Duration_data[names(Duration_data)[grepl("Nic_0710|Nic_0712", names(Duration_data))]]
vec_0.25 <- c()
# Loop through each vector in the list and concatenate
for (vec in df_0.25) {
  vec_0.25 <- c(vec_0.25, vec)
}
vec_0.25 <- data.frame(vec_0.25[vec_0.25 < 25])
colnames(vec_0.25)[1] <- "Duration_0.25"

length(Duration_data)


df_0.5 <- Duration_data[names(Duration_data)[grepl("Nic_0714|Nic_0716", names(Duration_data))]]
vec_0.5 <- c()
# Loop through each vector in the list and concatenate
for (vec in df_0.5) {
  vec_0.5 <- c(vec_0.5, vec)
}
vec_0.5 <- data.frame(vec_0.5[vec_0.5 < 25])
colnames(vec_0.5)[1] <- "Duration_0.5"



df_0.75 <- Duration_data[names(Duration_data)[grepl("Nic_0718|Nic_0720", names(Duration_data))]]
vec_0.75 <- c()
# Loop through each vector in the list and concatenate
for (vec in df_0.75) {
  vec_0.75 <- c(vec_0.75, vec)
}
vec_0.75 <- data.frame(vec_0.75[vec_0.75 < 25])
colnames(vec_0.75)[1] <- "Duration_0.75"


df_1 <- Duration_data[names(Duration_data)[grepl("Nic_0722|Nic_0724", names(Duration_data))]]
vec_1 <- c()
# Loop through each vector in the list and concatenate
for (vec in df_1) {
  vec_1 <- c(vec_1, vec)
}
vec_1 <- data.frame(vec_1[vec_1 < 25])
colnames(vec_1)[1] <- "Duration_1"


df_0.5.2 <- Duration_data[names(Duration_data)[grepl("Nic_0726|Nic_0728", names(Duration_data))]]
vec_0.5.2 <- c()
# Loop through each vector in the list and concatenate
for (vec in df_0.5.2) {
  vec_0.5.2 <- c(vec_0.5.2, vec)
}
vec_0.5.2 <- data.frame(vec_0.5.2[vec_0.5.2 < 25])
colnames(vec_0.5.2)[1] <- "Duration_1"


# HISTOGRAMS PER CONCENTRATION Nicotine
vecs <- list(vec_0.25, vec_0.5, vec_0.75, vec_1, vec_0.5.2)
vecnames <- c("0.25", "0.5", "0.75", "1", "0.5.2")
for (g in 1:length(vecs)){
  p <- ggplot(data = vecs[[g]], aes(x = vecs[[g]][, 1])) +
    xlim(0,5) +
    coord_cartesian(ylim = c(0,150)) +
    geom_histogram(binwidth = 0.05, fill = "skyblue", color = "black") +
    labs(title = paste("Histogram of", vecnames[g], "Nicotine Durations", sep = " "), x = "Values", y = "Frequency")
  print(p)
}



---------
  
df_0.25 <- Duration_data[names(Duration_data)[grepl("Con_0710|Con_0712", names(Duration_data))]]
vec_0.25 <- c()
# Loop through each vector in the list and concatenate
for (vec in df_0.25) {
  vec_0.25 <- c(vec_0.25, vec)
}
vec_0.25 <- data.frame(vec_0.25[vec_0.25 < 25])
colnames(vec_0.25)[1] <- "Duration_0.25"

length(Duration_data)


df_0.5 <- Duration_data[names(Duration_data)[grepl("Con_0714|Con_0716", names(Duration_data))]]
vec_0.5 <- c()
# Loop through each vector in the list and concatenate
for (vec in df_0.5) {
  vec_0.5 <- c(vec_0.5, vec)
}
vec_0.5 <- data.frame(vec_0.5[vec_0.5 < 25])
colnames(vec_0.5)[1] <- "Duration_0.5"



df_0.75 <- Duration_data[names(Duration_data)[grepl("Con_0718|Con_0720", names(Duration_data))]]
vec_0.75 <- c()
# Loop through each vector in the list and concatenate
for (vec in df_0.75) {
  vec_0.75 <- c(vec_0.75, vec)
}
vec_0.75 <- data.frame(vec_0.75[vec_0.75 < 25])
colnames(vec_0.75)[1] <- "Duration_0.75"


df_1 <- Duration_data[names(Duration_data)[grepl("Con_0722|Con_0724", names(Duration_data))]]
vec_1 <- c()
# Loop through each vector in the list and concatenate
for (vec in df_1) {
  vec_1 <- c(vec_1, vec)
}
vec_1 <- data.frame(vec_1[vec_1 < 25])
colnames(vec_1)[1] <- "Duration_1"


df_0.5.2 <- Duration_data[names(Duration_data)[grepl("Con_0726|Con_0728", names(Duration_data))]]
vec_0.5.2 <- c()
# Loop through each vector in the list and concatenate
for (vec in df_0.5.2) {
  vec_0.5.2 <- c(vec_0.5.2, vec)
}
vec_0.5.2 <- data.frame(vec_0.5.2[vec_0.5.2 < 25])
colnames(vec_0.5.2)[1] <- "Duration_1"

# HISTOGRAMS PER CONCENTRATION
vecs <- list(vec_0.25, vec_0.5, vec_0.75, vec_1, vec_0.5.2)
vecnames <- c("0.25", "0.5", "0.75", "1", "0.5.2")
for (g in 1:length(vecs)){
  p <- ggplot(data = vecs[[g]], aes(x = vecs[[g]][, 1])) +
    xlim(0,5) +
    coord_cartesian(ylim = c(0,150)) +
    geom_histogram(binwidth = 0.05, fill = "skyblue", color = "black") +
    labs(title = paste("Histogram of", vecnames[g], "Control Durations", sep = " "), x = "Values", y = "Frequency")
  print(p)
}



vec_0.25$Duration_0.25[1:996] <- scan()
write.table(vec_0.5.2, "clipboard", sep = "\t", row.names = FALSE, col.names = FALSE)
