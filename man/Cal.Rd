\name{Cal}

\alias{Cal}
\alias{BarPlot}
\alias{GrowthCurve}

\title{
CREATE BAR PLOT OR BACTERIAL GROWTH CURVE
}
\description{
Cal calculates mean and standard deviation of imported data. 
BarPlot creates bar plot for normal phenotypic assays in several testing groups. 
GrowthCurve creates line chart to show bacterial growth condition. 
}

\usage{
Cal(myData, rep = 3, measurementPos, strainName, conditionName)
BarPlot <- function (myData, rep = 3, measurementPos, strainName, conditionName, ylabName)
GrowthCurve <- function (myData, rep = 3, measurementPos, strainName, conditionName)
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
  \item{ylabname}{
Name of y-axis in exported bar plot
}
}

\value{
Cal returns a list. 
\item{comp_1}{mean of each tested group}
\item{comp_2}{standard deviation of each tested group}
BarPlot a bar plot indicating the difference of several bacterial strains in several conditions. 
GrowthCurve a line chart indicating the growth condition of several bacterial strains. 
}

\author{
Menghao Yu
}

\examples{
Test1 <- data(Test1)
CalResult <- Cal(myData = Test1, rep = 3, measurementPos = 3, strainName = c("WT", "clpXP", "clpXP(pXP)"), conditionName = c("24", "48"))
BarPlot <- function (myData =  Test1, rep = 3, measurementPos = 3, strainName = c("WT", "clpXP", "clpXP(pXP)"), conditionName = c(24, 48), ylabName = "Distance (mm)")

Test2 <- data(Test2)
GrowthCurve <- function (myData = Test2, rep = 3, measurementPos = 3, strainName = c("WT(p)", "#3(p)", "#3(pT)"), conditionName = c(3, 6, 9, 12, 15, 18, 21, 24))
}