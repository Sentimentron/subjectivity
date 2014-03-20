# Does word-sense disambiguation make a difference?
require('data.table')
require('caTools')

# Read the data 
dataCsv <- read.csv('data.csv')

# Work out the number of subjective annotations
subjTotal <- sum(dataCsv[, c('pos','neg','neu')])
totalAnns <- sum(dataCsv[, 'total'])

# Get the data which have synsets...
dataCsv <- dataCsv[dataCsv[,"synset"] != "",]

# Find all the synsets
allSynsets <- unique(dataCsv$synset)
allWords   <- unique(dataCsv$token)

# Relevant data 
relevantData <- data.frame()
for (word in allWords) {
    
    subData <- dataCsv[dataCsv$token == word,]
    if (length(unique(subData$synset)) > 1) {
        relevantData <- rbind(subData, relevantData)
    }

}

# Testable words 
someWords <- unique(relevantData$token)

# Create baseline sequence
gammaSeq <- seq(1,100)/100.0

modelInt <- function(subData) { 
    totalObservations <- sum(subData$total)
    totalSubjective <- sum(subData[,c("neg","neu","pos")])

    if (totalObservations == 0) return (NaN)
    if (totalSubjective == 0) return (NaN)

    M1 <- dbeta(gammaSeq, 2 + totalSubjective, 2 + totalObservations - totalSubjective)
    M1Int <- trapz(gammaSeq, M1)
    return (M1Int)
}

for(word in someWords) {

    subData <- dataCsv[dataCsv$token == word,]
    if(nrow(subData) < 2) next
    M1Int <- modelInt(subData)
    subSynsets <- unique(subData$synset)
    for (synset in subSynsets) {
        subSubData <- subData[subData$synset == synset,]
        M2Int <- modelInt(subSubData)
        print(sprintf('%s\t%s\t%.4f\t%.4f\n',word, synset, M1Int, M2Int))
    }
}
