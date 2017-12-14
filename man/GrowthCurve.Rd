\name{GrowthCurve}

\alias{GrowthCurve}

\title{
Create Bacterial Growth Curve
}

\description{
GrowthCurve creates line chart to show bacterial growth condition. 
}

\usage{
GrowthCurve(myData, rep = 3, measurementPos, strainName, conditionName)
}

\arguments{
  \item{myData}{
Imported experiment data
}
  \item{rep}{
Replication of each experiment setting
}
  \item{measurementPos}{
Number of the column which stores measurement
}
  \item{strainName}{
Name of tested strains
}
  \item{conditionName}{
Tested conditions
}
}

\value{
GrowthCurve a line chart indicating the growth condition of several bacterial strains. 
}

\author{
Menghao Yu
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (myData, rep = 3, measurementPos, strainName, conditionNam) 
{
    myMeanSD <- Cal(myData, rep = 3, measurementPos = 3, strainName, 
        conditionName)
    myMean <- myMeanSD[[1]]
    meanMatrix <- matrix(myMean, nrow = length(strainName), ncol = length(conditionName), 
        byrow = TRUE, dimnames = list(strainName, conditionName))
    i <- 1
    while (i <= length(strainName)) {
        if (i == 1) {
            plot(x = colnames(meanMatrix), y = meanMatrix[1, 
                ], type = "o", xlab = "Time(h)", ylab = "OD600", 
                pch = 1)
        }
        else {
            points(x = colnames(meanMatrix), y = meanMatrix[i, 
                ], type = "o", pch = i)
        }
        i <- i + 1
    }
    box()
  }
  
Test2 <- data(Test2)
GrowthCurve <- function (myData = Test2, rep = 3, measurementPos = 3, strainName = c("WT(p)", "#3(p)", "#3(pT)"), conditionName = c(3, 6, 9, 12, 15, 18, 21, 24))
}
