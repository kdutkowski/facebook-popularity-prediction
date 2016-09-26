if (!require("ggplot2")) install.packages("ggplot2", repos="http://cran.rstudio.com/")

library(ggplot2)

# Read all data from file into a data frame
rawData <- read.csv('data/data.csv', header = FALSE)

removeOutliers <- function(dataFrame, colIndex) {
  standardDeviation <- sd(dataFrame[, colIndex])
  sigma3.max <- mean(dataFrame[, colIndex]) + 3 * standardDeviation
  sigma3.min <- mean(dataFrame[, colIndex]) - 3 * standardDeviation
  
  return(dataFrame[dataFrame[, colIndex] > sigma3.min & dataFrame[, colIndex] < sigma3.max,])
}

# Analyse the basic statistics of the v(n) or n = 24, 72, 168.
summary(rawData[, c(24, 72, 168) + 1])

# Extract the values of v(168)
dataWithoutOutliers <- removeOutliers(rawData, 169)

# Plot distribution histogram 
ggplot(rawData, aes(x=rawData[, 169])) + 
  geom_histogram(bins = 50)

# Plot logx distribution histogram
ggplot(rawData, aes(x=rawData[, 169])) + 
  geom_histogram(bins = 50) +
  scale_x_log10()

# Plot logx distribution histogram without outliers
ggplot(dataWithoutOutliers, aes(x=dataWithoutOutliers[, 169])) + 
  geom_histogram(bins = 50) +
  scale_x_log10()
