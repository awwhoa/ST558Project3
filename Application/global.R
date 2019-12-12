
# Read in the data file to use
data <- as.data.frame(unclass(read.csv('https://raw.githubusercontent.com/awwhoa/ST558Project3/master/Data/AnalysisDatasetSmall.csv',stringsAsFactors = F,header=T)))

# Create subset of numeric variables from analysis dataset
numSubset <- as.data.frame(unclass(dplyr::select(data, c("Total.Passengers","Total.Injuries","Fatal"))))

# Create subset of numeric variables from analysis dataset
numPredSubset <- as.data.frame(unclass(dplyr::select(data, c("Total.Passengers","Total.Injuries"))))

# Create subset of categorical variables from analysis dataset
catSubset <- as.data.frame(unclass(dplyr::select(data, -c("Total.Passengers","Total.Injuries","Fatal"))))

# Create subset of predictor variables from analysis dataset
predSubset <- as.data.frame(unclass(dplyr::select(data, -c("Fatal"))))

# Create testing and training data
sampleSize <- floor(0.8 * nrow(data))
set.seed(123)
trainInd <- sample(seq_len(nrow(data)), size = sampleSize)
train <- data[trainInd, ]
test <- data[-trainInd, ]

# Create data frames of 
