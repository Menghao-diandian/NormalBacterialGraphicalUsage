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
  
  # No missing data
  if (sum(is.na(Measurement)) != 0) {
    stop ("No missing data is allowed!")
  }
  
  # All experiment should have the same number of replication
  if ((measurementNumber %% rep) != 0) {
    stop ("Mismatches exist in imported data, 
          please import the results of all replication!")
  }
  
  # Non-necessary data is allowed
  if (measurementNumber != CalMeasurementNumber) {
    warning ("Possible mismatches exist in imported data, 
             please ignore this if not all data is used in your plot.")
  }
  
  return (myfile)
}

CalMean <- function (myData,
                     measurementPos,
                     StrainColName = "Strain",
                     conditionColName,
                     strainName,
                     conditionName){
  # This function is used to calculate the mean of each experiment setting 
  # and return a matrix of this mean result
  # StrainColName stores the name of the column which stores strain name
  # conditionColName stores the name of the column which stores testing condition

  meanMatrix <- tapply (myData[[measurementPos]], 
                        INDEX = list (myData[[StrainColName]], 
                                      myData[[conditionColName]]), 
                        mean)
  
  meanMatrix <- meanMatrix[strainName, 
                           as.character(conditionName)]
  
  return (meanMatrix)
}

CalSD <- function (myData,
                   measurementPos, 
                   StrainColName = "Strain",
                   conditionColName,
                   strainName,
                   conditionName) {
  # This function is used to calculate the standard deviation of each experiment setting 
  # and return a matrix of this standard deviation result
  
  SDMatrix <- tapply (myData[[measurementPos]], 
                      INDEX = list (myData[[StrainColName]], 
                                    myData[[conditionColName]]), 
                      sd)
  
  SDMatrix <- SDMatrix[strainName,
                       as.character(conditionName)]

   return (SDMatrix)
 }

BarPlot <- function (myData, 
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
                         measurementPos, 
                         StrainColName,
                         conditionColName,
                         strainName,
                         conditionName = timePoint)
  
  # Draw the growth curve
  i <- 1
  while (i <= length(strainName)){ # draw the growth curve of the 1st strain
    if (i == 1){
      plot(x = colnames(meanMatrix), 
           y = meanMatrix[1,], 
           type = "o", 
           xlab = "Time(h)", 
           ylab = "OD600", 
           ylim = c(0, max(meanMatrix)*1.2),
           pch = 1, 
           xaxt="n")
      legend (x = min(timePoint), 
              y = max(meanMatrix), 
              legend = strainName[1], 
              pch = 1, bty = "n")
      axis (side = 1, at = timePoint)
    } else { # draw the growth curve of the i th strain
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
