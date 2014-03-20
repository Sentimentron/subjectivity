# Estimate the beta conjugate prior

data <- read.csv('data.csv')

# Distribution properties
initialAlpha <- 2.0
initialBeta <- 2.0

# Counters
n <- 0          # Number of observations processed
counter <- 0 # Which observation row we're on 
subjSum <- 0

# Colours for plotting the curve
greyColours <- gray.colors(length(data)+1)

# Curve values
x <- seq(1,100)/100.0
curve(dbeta(x, initialAlpha, initialBeta),ylim=c(0,5))

alphaEstimate <- initialAlpha
betaEstimate <- initialBeta

for (i in 1:nrow(data)) {
    row <- data[i,]
    subj <- sum(row[c('pos','neg','neu')])
    t <- sum(row['total'])
    subjSum <- subjSum + subj
    n <- n + t
    counter <- counter + 1
    alphaEstimate <- initialAlpha + subjSum
    betaEstimate <- initialBeta + n - subjSum
#    print(alphaEstimate)
#    print(betaEstimate)
    if (i %% 2 == 0) {
        lines(x, dbeta(x, alphaEstimate, betaEstimate), col=greyColours[counter])
    }
    if (i > 1000) break 
}

