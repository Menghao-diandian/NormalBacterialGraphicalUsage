\name{CalSD}

\alias{CalSD}

\title{
Calculate Mean
}
\description{
CalSD calculates standard deviation of imported data
}
\usage{
CalSD(myData, rep = 3, measurementPos, strainName, conditionName)
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
CalSD returns standard deviation of each tested group
}

\author{
Menghao Yu
}

\examples{
data(Test1)
CalSD (Test1, rep = 3, measurementPos = 3, strainName = c("WT", "clpXP", "clpXP(pXP)"), conditionName = c(24, 48))
}

