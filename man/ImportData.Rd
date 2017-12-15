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
data(Test1)
ImportData(Test1)
}