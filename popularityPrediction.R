if (!require("ggplot2")) install.packages("ggplot2", repos="http://cran.rstudio.com/")
if (!require("reshape2")) install.packages("reshape2", repos="http://cran.rstudio.com/")
if (!require("caret")) install.packages("caret", repos="http://cran.rstudio.com/")

library(ggplot2)
library(reshape2)
library(caret)

# Read all data from file into a data frame
rawData <- read.csv('data/data.csv', header = FALSE)

removeOutliers <- function(dataFrame, colIndex) {
  standardDeviation <- sd(dataFrame[, colIndex])
  sigma3.max <- mean(dataFrame[, colIndex]) + 3 * standardDeviation
  sigma3.min <- mean(dataFrame[, colIndex]) - 3 * standardDeviation
  
  return(dataFrame[dataFrame[, colIndex] > sigma3.min & dataFrame[, colIndex] < sigma3.max,])
}

rMse <- function(trueValue, predictedValue) {
  return(mean((predictedValue/trueValue - 1)^2))
}

# Analyse the basic statistics of the v(n) or n = 24, 72, 168.
summary(rawData[, c(24, 72, 168) + 1])

# Plot distribution histogram 
ggplot(rawData, aes(x=rawData[, 169])) + 
  geom_histogram(bins = 50)

# Plot logx distribution histogram
rawData$log169 <- log(rawData[, 169])
ggplot(rawData, aes(x=rawData$log169)) + 
  geom_histogram(bins = 50)

# Extract the values of v(168)
dataWithoutOutliers <- removeOutliers(rawData, "log169")

# Plot logx distribution histogram without outliers
ggplot(dataWithoutOutliers, aes(x=dataWithoutOutliers$log169)) + 
  geom_histogram(bins = 50)

# Correlation between inputs 1:24 and output
regressionParametersIndices = 1:24 + 1
correlations <- sapply(dataWithoutOutliers[, regressionParametersIndices], function (x) { cor(x, dataWithoutOutliers$log169) })

# Split data to train and test
inTraining <- createDataPartition(dataWithoutOutliers$log169, p = .9, 
                                  list = FALSE)
trainData <- dataWithoutOutliers[ inTraining,]
testData <- dataWithoutOutliers[-inTraining,]

results <- data.frame(time = 1:length(regressionParametersIndices))

results$singleInput <- sapply(regressionParametersIndices, function(x) {
  model <- lm(as.formula(paste("log169 ~", colnames(trainData)[x])), data = trainData)
  predicted <- predict(model, testData)
  return(rMse(testData$log169, predicted))
})

results$multipleInputs <- sapply(regressionParametersIndices, function(x) {
  model <- lm(as.formula(paste("log169 ~", paste(colnames(trainData)[2:x], collapse = "+"))), data = trainData)
  predicted <- predict(model, testData)
  return(rMse(testData$log169, predicted))
})

meltedResults <- melt(results, id="time")
ggplot(data=meltedResults, aes(x=time, y=value, colour=variable, shape=variable)) +
        geom_point() +
        geom_line()
