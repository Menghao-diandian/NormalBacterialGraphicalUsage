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
data(Test1)
BarPlot (Test1, rep = 3, measurementPos = 3, strainName = c("WT", "clpXP", "clpXP(pXP)"), conditionName = c(24, 48), ylabName = "Distance (mm)")
}