ImportData <- function (myfile, 
                        rep = 3,
                        measurementPos,
                        strainName,
                        conditionName){
  # This funciton is used to import experiment data 
  # and check the imported data for problems. 
  # myfile is the name of imported file, rep is the replication number of 
  # each experiment setting, measurementPos is the location of measurement 
  # in the file, strainName is the name of tested bacterial strains, 
  # conditionName is the conditon where the results are measured. 
  # In this function, non-neccessary data are allowed to import, however, 
  # all experment setting must include the same number of replications, 
  # no missing is allowed.  
  myfile <- read.csv(myfile, header = TRUE) # Import data
  
  # Imported data should include proper number of data
  measurementNumber <- length(myfile[[measurementPos]])
  CalMeasurementNumber <- rep * length(strainName)*length(conditionName)
  Measurement <- myfile[[measurementPos]]
  if (sum(is.na(Measurement)) != 0) {
    stop ("No missing data is allowed!")
  }
  if ((measurementNumber %% rep) != 0) {
    stop ("Mismatches exist in imported data, 
          please import the results of all replication!")
  }
  if (measurementNumber != CalMeasurementNumber) {
    warning ("Possible mismatches exist in imported data, 
             please ignore this if not all data is used in your plot.")
  }
  return (myfile)
}

CalMean <- function (myData,
                     rep = 3, 
                     measurementPos,
                     StrainColName = "Strain",
                     conditionColName, 
                     strainName,
                     conditionName){
  # This function is used to calculate the mean of each experiment setting 
  # and return a matrix of this mean result
  # StrainColName stores the name of the column which stores strain name
  # conditionColName stores the name of the column which stores testing condition
  meanMatrix <- matrix( , length(strainName), 
                        length(conditionName),
                        dimnames = list (strainName, conditionName))
  i <- 1
  j <- 1
  while (i <= length(strainName)){
    while (j <= length(conditionName)){
      meanMatrix[i, j] <- tapply(myData[[measurementPos]],
                                 (myData[StrainColName] == strainName[i])
                                 & (myData[conditionColName] == conditionName[j]),
                                 mean)[2]
      j <- j+1
    }
    i <- i+1
    j <- 1
  }
  return (meanMatrix)
}

CalSD <- function (myData,
                   rep = 3, 
                   measurementPos, 
                   StrainColName = "Strain",
                   conditionColName,
                   strainName,
                   conditionName) {
  # This function is used to calculate the standard deviation of each experiment setting 
  # and return a matrix of this standard deviation result
  SDMatrix <- matrix( , length(strainName), 
                        length(conditionName),
                        dimnames = list (strainName, conditionName))
  i <- 1
  j <- 1
  while (i <= length(strainName)){
    while (j <= length(conditionName)){
      SDMatrix[i, j] <- tapply(myData[[measurementPos]],
                               (myData[StrainColName] == strainName[i])
                               & (myData[conditionColName] == conditionName[j]),
                               sd)[2]
      j <- j+1
    }
    i <- i+1
    j <- 1
  }
  return (SDMatrix)
}

BarPlot <- function (myData, 
                     rep = 3, 
                     measurementPos, 
                     StrainColName = "Strain",
                     conditionColName,
                     strainName,
                     conditionName, 
                     ylabName) {
  # This funciton is used to draw barplot to 
  # compare the difference between each experiment setting
  # ylabName is the name of y-axis in exported bar plot
  
  # Get the mean matrix
  meanMatrix <- CalMean (myData, 
                         rep, 
                         measurementPos, 
                         StrainColName,
                         conditionColName,
                         strainName,
                         conditionName)
  # Draw the bar plot
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
                         StrainColName = "Strain",
                         conditionColName,
                         strainName,
                         timePoint) {
  # This function is used to draw growth curve to 
  # compare the difference between different strains
  # timePoint is the time point at which OD600 is measure
  
  # Get the mean matrix
  meanMatrix <- CalMean (myData,
                         rep = 3, 
                         measurementPos, 
                         StrainColName,
                         conditionColName,
                         strainName,
                         conditionName = timePoint)
  
  # Draw the growth curve
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
      legend (x = min(timePoint), 
              y = max(meanMatrix), 
              legend = strainName[1], 
              pch = 1, bty = "n")
      axis (side = 1, at = timePoint)
      } else {
        points(x = colnames(meanMatrix), 
               y = meanMatrix[i,], 
               type = "o", 
               pch = i)
        legend (x = min(timePoint), 
                y = max(meanMatrix)-(i-1)*0.2, 
                        legend = strainName[i], 
                        pch = i, bty = "n")
        }
    i <- i + 1
  }
  box()
}
