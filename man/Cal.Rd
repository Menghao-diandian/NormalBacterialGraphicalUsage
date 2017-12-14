\name{Cal}

\alias{Cal}

\title{
Calculate mean and sd 
}
\description{
Cal calculates mean and standard deviation of imported data. 
}

\usage{
Cal(myData, rep = 3, measurementPos, strainName, conditionName)
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
Cal returns a list. 
\item{comp_1}{mean of each tested group}
\item{comp_2}{standard deviation of each tested group}
}

\author{
Menghao Yu
}

\examples{
CalResult <- Cal(data(Test1), rep = 3, measurementPos = 3, strainName = c("WT", "clpXP", "clpXP(pXP)"), conditionName = c("24", "48"))
}