times <- tc_list[[1]]
# Load the matrixprofiler package
library(matrixprofiler)

# Example time series data (replace this with your actual time series data)
time_series <- tc_list[[1]]
time_series <- unlist(time_series)
time_series <- as.numeric(time_series)
time_series <- scale(time_series) 

  install.packages("tsmp")
library(tsmp)

# Compute the matrix profile using tsmp
mp_result <- tsmp(time_series, window_size = 4, exclusion_zone = 1/2)
plot(mp_result$mp, type = "l", main = "Matrix Profile", 
     xlab = "Index", ylab = "Matrix Profile Value")


length(time_series)
print(tc_list[[1]])
