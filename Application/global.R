
# Read in the data file to use
data <- as.data.frame(unclass(read.csv('https://raw.githubusercontent.com/awwhoa/ST558Project3/master/Data/AnalysisDatasetSmall.csv',stringsAsFactors = F,header=T)))


# Subset only numeric variables from dataset
numSubset <- as.data.frame(unclass(select(data, c("Total.Passengers","Total.Injuries","Fatal"))))


# Subset only numeric variables from dataset
catSubset <- as.data.frame(unclass(select(data, -c("Total.Passengers","Total.Injuries","Fatal"))))


testDownData <- data