\name{Test2}
\alias{Test2}
\docType{data}
\title{
BACTERIAL GROWTH DATA
}
\description{
  This dataset gives the bacterial growth data over 24 h, including 72 observation.
  3 replications in each time point. 
}
\usage{data("Test2")}
\format{
  A data frame with 72 observations on the following 3 variables.
  \describe{
    \item{\code{Strain}}{a character vector containing the name of tested strains}
    \item{\code{Time}}{a numeric vector containing measuring timepoint in hour}
    \item{\code{OD600}}{a numeric vector containing measuring results in OD600, 
    OD600 itself is a normal unit in bacterial growth measuring}
  }
}
\keyword{datasets}