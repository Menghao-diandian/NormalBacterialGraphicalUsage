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
test1 <- c(1:5, "6,7", "8,9,10")
tf <- tempfile()
writeLines(test1, tf)

ImportData(tf)
}