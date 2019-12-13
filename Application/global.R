
# Read in the data file to use
data <- as.data.frame(unclass(read.csv('https://raw.githubusercontent.com/awwhoa/ST558Project3/master/Data/AnalysisDatasetSmall.csv',stringsAsFactors = F,header=T)))

# Create subset of numeric variables from analysis dataset
numSubset <- as.data.frame(unclass(dplyr::select(data, c("Total.Passengers","Total.Injuries","Fatal"))))

# Create subset of numeric variables from analysis dataset
numPredSubset <- as.data.frame(unclass(dplyr::select(data, c("Total.Passengers","Total.Injuries"))))

# Create subset of categorical variables from analysis dataset
catSubset <- as.data.frame(unclass(dplyr::select(data, -c("Total.Passengers","Total.Injuries","Fatal"))))

# Create subset of predictor variables from analysis dataset
predSubset <- names(as.data.frame(unclass(dplyr::select(data, -c("Fatal")))))

# Create testing and training data
sampleSize <- floor(0.8 * nrow(data))
set.seed(123)
trainInd <- sample(seq_len(nrow(data)), size = sampleSize)
train <- data[trainInd, ]
test <- data[-trainInd, ]

# Create data frames of attribute choices for the logistic regression model predictions
dmg <- as.data.frame(unclass(dplyr::select(data, Aircraft.Damage)))
cat <- as.data.frame(unclass(dplyr::select(data, Aircraft.Category))) 
blt<- as.data.frame(unclass(dplyr::select(data, Amateur.Built)))
purp<- as.data.frame(unclass(dplyr::select(data, Purpose.of.Flight)))
wthr<- as.data.frame(unclass(dplyr::select(data, Weather.Condition)))
phase<- as.data.frame(unclass(dplyr::select(data, Broad.Phase.of.Flight)))
eng<- as.data.frame(unclass(dplyr::select(data, Engine.Count)))

# Read in the small data dictionary
dictionary <- as.data.frame(unclass(read.csv('https://raw.githubusercontent.com/awwhoa/ST558Project3/master/Data/DataDictionarySmall.csv', stringsAsFactors = F, header=T)))







