ImportData <- function (myfile){
  # Import experiment data 
  file <- read.csv(myfile, header = TRUE)
  return (file)
}

CalMean <- function (myData,
                     rep = 3, 
                     measurementPos, 
                     strainName,
                     conditionName){
  Measurement <- (myData[measurementPos])[,1]
  myMean <- c()
  while (length(Measurement) > 0){
    thisMean <- mean (Measurement[1:rep])
    Measurement <- Measurement [-(1:rep)]
    myMean <- c(myMean, thisMean)
  }
  meanMatrix <- matrix (myMean, 
                        nrow = length(strainName), 
                        ncol = length(conditionName), 
                        byrow = TRUE, 
                        dimnames = list (strainName, conditionName))
  return (meanMatrix)
}

CalSD <- function (myData,
                   rep = 3, 
                   measurementPos, 
                   strainName,
                   conditionName) {
  Measurement <- myData[measurementPos][,1]
  mySD <- c()
  while (length(Measurement) > 0){
    thisSD <- sd (Measurement[1:rep])
    Measurement <- Measurement [-(1:rep)]
    mySD <- c(mySD, thisSD)
  }
  SDMatrix <- matrix (mySD,
                      nrow = length(strainName), 
                      ncol = length(conditionName), 
                      byrow = TRUE, 
                      dimnames = list (strainName, conditionName))
  return (SDMatrix)
}

BarPlot <- function (myData, 
                     rep = 3, 
                     measurementPos, 
                     strainName,
                     conditionName, 
                     ylabName) {
  meanMatrix <- CalMean (file, 
                         rep, 
                         measurementPos, 
                         strainName,
                         conditionName)
  
  barplot(t(meanMatrix), 
          ylab = ylabName, 
          legend = conditionName, 
          col=heat.colors(length(conditionName)), 
          beside = TRUE,
          ylim = c(0, 1.5*(max(as.vector(meanMatrix)))))
  box()
}

GrowthCurve <- function (myData, 
                         rep = 3, 
                         measurementPos, 
                         strainName,
                         timePoint) {
  
  meanMatrix <- CalMean (myData, 
                         rep = 3, 
                         measurementPos, 
                         strainName,
                         conditionName = timePoint)
  i <- 1
  while (i <= length(strainName)){
    if (i == 1){
      plot(x = colnames(meanMatrix), 
           y = meanMatrix[1,], 
           type = "o", 
           xlab = "Time(h)", 
           ylab = "OD600", 
           pch = 1, 
           xaxt="n")
      legend (x = 2, y = 1.1, legend = strainName[1], pch = 1, bty = "n")
      axis (side = 1, at = timePoint)
    }
   else {
     points(x = colnames(meanMatrix), 
          y = meanMatrix[i,], 
          type = "o", 
          pch = i)
     legend (x = 2, y = 1.1-(i-1)*0.2, legend = strainName[i], pch = i, bty = "n")
   }
    i <- i + 1
  }
  box()
}
