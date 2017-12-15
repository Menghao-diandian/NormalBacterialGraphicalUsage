\name{GrowthCurve}

\alias{GrowthCurve}

\title{
Create Bacterial Growth Curve
}

\description{
GrowthCurve creates line chart to show bacterial growth condition. 
}

\usage{
GrowthCurve(myData, rep = 3, measurementPos, strainName, timePoint)
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
GrowthCurve (Test2, rep = 3, measurementPos = 3, strainName = c("WT(p)", "#3(p)", "#3(pT)"), timePoint = c(3, 6, 9, 12, 15, 18, 21, 24))
}
