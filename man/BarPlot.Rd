\name{BarPlot}

\alias{BarPlot}

\title{
CREATE BAR PLOT 
}

\description{
BarPlot creates bar plot for normal phenotypic assays in several testing groups.
}

\usage{
BarPlot(myData, rep = 3, measurementPos, strainName, conditionName, ylabName)
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
  \item{ylabName}{
Name of y-axis in exported bar plot
}
}

\value{
BarPlot a bar plot indicating the difference of several bacterial strains in several conditions.
}

\author{
Menghao Yu
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (myData, rep = 3, measurementPos, strainName, conditionName, 
    ylabName) 
{
    myMeanSD <- Cal(myData, rep = 3, measurementPos, strainName, 
        conditionName)
    myMean <- myMeanSD[[1]]
    meanMatrix <- matrix(myMean, nrow = length(strainName), ncol = length(conditionName), 
        byrow = TRUE, dimnames = list(strainName, conditionName))
    barplot(t(meanMatrix), ylab = ylabName, legend = conditionName, 
        col = heat.colors(length(conditionName)), beside = TRUE, 
        ylim = c(0, 1.5 * (max(myMean))))
    box()
  }

Test1 <- data(Test1)
BarPlot <- function (myData =  Test1, rep = 3, measurementPos = 3, strainName = c("WT", "clpXP", "clpXP(pXP)"), conditionName = c(24, 48), ylabName = "Distance (mm)")

}