\name{ImportData}

\alias{ImportData}
\title{
IMPORT EXPRERIMENT RESULTS
}

\description{
Import experiment results. 
}

\usage{
ImportData(myfile)
}

\arguments{
  \item{myfile}{
myfile is the name of a csv file, the file should currently under working directory
}
}

\value{
Return the content of the imported data
}

\author{
Menghao Yu
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (myfile) 
{
    myData <- read.csv(myfile, header = TRUE)
    return(myData)
  }
  
ImportData(data(Test1))
}