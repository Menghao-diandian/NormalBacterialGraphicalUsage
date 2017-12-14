ImportData <- function (myfile){
  # Import experiment data 
  myData <- read.csv(myfile, header = TRUE)
  return (myData)
}


Cal <- function (myData, 
                 rep = 3, 
                 measurementPos, 
                 strainName,
                 conditionName){
  # Calculate mean
  Measurement <- myData[measurementPos]#[,1]
  myMean <- c()
  while (length(Measurement) > 0){
    thisMean <- mean (Measurement[1:rep])
    Measurement <- Measurement [-(1:rep)]
    myMean <- c(myMean, thisMean)
  }
  # Calculate sd
  Measurement <- myData[measurementPos]#[,1]
  mySD <- c()
  while (length(Measurement) > 0){
    thisSD <- sd (Measurement[1:rep])
    Measurement <- Measurement [-(1:rep)]
    mySD <- c(mySD, thisSD)
  }
#  sdMatrix <- matrix (mySD, 
#                      nrow = length(strainName), 
#                      ncol = length(conditionName), 
#                      byrow = TRUE, 
#                      dimnames = list (strainName, conditionName))
  return (list (myMean, mySD))
}

# Bar plot
BarPlot <- function (myData, 
                     rep = 3, 
                     measurementPos, 
                     strainName,
                     conditionName, 
                     ylabName) {
  myMeanSD <- Cal(myData,
                  rep=3, 
                  measurementPos, 
                  strainName,
                  conditionName)
  myMean <- myMeanSD[[1]]
  meanMatrix <- matrix (myMean, 
                        nrow = length(strainName), 
                        ncol = length(conditionName), 
                        byrow = TRUE, 
                        dimnames = list (strainName, conditionName))
  barplot(t(meanMatrix), 
          ylab = ylabName, 
          legend = conditionName, 
          col=heat.colors(length(conditionName)), 
          beside = TRUE,
          ylim = c(0, 1.5*(max(myMean))))
  box()
}

# Growth curve 
GrowthCurve <- function (myData, 
                         rep = 3, 
                         measurementPos, 
                         strainName,
                         conditionNam) {
  myMeanSD <- Cal(myData,
                  rep=3, 
                  measurementPos=3, 
                  strainName,
                  conditionName)
  myMean <- myMeanSD[[1]]
  meanMatrix <- matrix (myMean, 
                        nrow = length(strainName), 
                        ncol = length(conditionName), 
                        byrow = TRUE, 
                        dimnames = list (strainName, conditionName))
  i <- 1
  while (i <= length(strainName)){
    if (i == 1){
      plot(x = colnames(meanMatrix), 
           y = meanMatrix[1,], 
           type = "o", 
           xlab = "Time(h)", 
           ylab = "OD600", 
           pch = 1)
    }
   else {
     points(x = colnames(meanMatrix), 
          y = meanMatrix[i,], 
          type = "o", 
          pch = i)
   }
    i <- i + 1
  }
  box()
}
