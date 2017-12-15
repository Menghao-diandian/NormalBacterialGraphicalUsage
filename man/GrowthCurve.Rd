\name{GrowthCurve}

\alias{GrowthCurve}

\title{
CREATE BACTERIAL GROWTH CURVE
}

\description{
  GrowthCurve creates line chart to show bacterial growth condition. 
}

\usage{
GrowthCurve(myData, rep = 3, measurementPos, strainName, timePoint)
}

\arguments{
  \item{myData}{
Imported experiment data, refer to data(Test2) for proper format
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
  \item{timePoint}{
Time point at which OD600 is measured
}
}

\value{
GrowthCurve a line chart indicating the growth condition of several bacterial strains. 
}

\author{
Menghao Yu
}

\examples{
data(Test2)
a <- c("WT(p)", "#3(p)", "#3(pT)")
b <- c(3, 6, 9, 12, 15, 18, 21, 24)
GrowthCurve (Test2, rep = 3, measurementPos = 3, strainName = a, timePoint = b)
}
