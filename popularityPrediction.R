if (!require("ggplot2"))
  install.packages("ggplot2", repos = "http://cran.rstudio.com/")
if (!require("reshape2"))
  install.packages("reshape2", repos = "http://cran.rstudio.com/")
if (!require("caret"))
  install.packages("caret", repos = "http://cran.rstudio.com/")

library(ggplot2)
library(reshape2)
library(caret)

# Read all data from file into a data frame
rawData <- read.csv('data/data.csv', row.names = 1, header = FALSE, col.names = c("id", paste("h", 1:168, sep = "")))

# A function that removes outliers from data frame according to
# 3-sigma rule, based on column specified with colIndex
removeOutliers <- function(dataFrame, colIndex) {
  standardDeviation <- sd(dataFrame[, colIndex])
  sigma3.max <- mean(dataFrame[, colIndex]) + 3 * standardDeviation
  sigma3.min <- mean(dataFrame[, colIndex]) - 3 * standardDeviation
  
  return(dataFrame[dataFrame[, colIndex] > sigma3.min &
                     dataFrame[, colIndex] < sigma3.max, ])
}

# A function used to calculate mean Relative Squared Error
rMse <- function(trueValue, predictedValue) {
  return(mean((predictedValue / trueValue - 1) ^ 2))
}

# Analyse the basic statistics of the v(n) or n = 24, 72, 168.
statisticsParametersIndices <- c(24, 72, 168)
summary(rawData[, statisticsParametersIndices])

# Plot distribution histogram
ggplot(rawData, aes(x = rawData[, 168])) +
  geom_histogram(bins = 50)

# Plot logx distribution histogram
rawData$log168 <- log(rawData[, 168])
ggplot(rawData, aes(x = rawData$log168)) +
  geom_histogram(bins = 50)

# Extract the values of v(168)
dataWithoutOutliers <- removeOutliers(rawData, "log168")

# Plot logx distribution histogram without outliers
ggplot(dataWithoutOutliers, aes(x = dataWithoutOutliers$log168)) +
  geom_histogram(bins = 50)

# Correlation between inputs 1:24 and output
regressionParametersIndices = 1:24
correlations <- sapply(dataWithoutOutliers[, regressionParametersIndices], function (x) {
  cor(x, dataWithoutOutliers$log168)
})

# Split data to train and test
inTraining <- createDataPartition(dataWithoutOutliers$log168, p = .9,
                                  list = FALSE)
trainData <- dataWithoutOutliers[inTraining, ]
testData <- dataWithoutOutliers[-inTraining, ]

# Initialize the data frame for results
results <- data.frame(time = regressionParametersIndices)

# For each reference time create the single parameter model and calculate rMSE
results$singleInput <- sapply(regressionParametersIndices, function(x) {
  model <-
    lm(as.formula(paste("log168 ~", colnames(trainData)[x])), data = trainData)
  predicted <- predict(model, testData)
  return(rMse(testData$log168, predicted))
})

# The same for model built on all views preceding the reference time
results$multipleInputs <- sapply(regressionParametersIndices, function(x) {
  model <-
    lm(as.formula(paste(
      "log168 ~", paste(colnames(trainData)[1:x], collapse = "+")
    )), data = trainData)
  predicted <- predict(model, testData)
  return(rMse(testData$log168, predicted))
})

# Plot mRSE for both models
meltedResults <- melt(results, id = "time")
ggplot(data = meltedResults,
       aes(
         x = time,
         y = value,
         group = variable,
         colour = variable,
         shape = variable
       )) +
  labs(y = "mRSE",
       x = "Reference time (n)") +
  geom_point() +
  geom_line()
