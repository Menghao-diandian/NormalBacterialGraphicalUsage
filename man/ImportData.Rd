\name{ImportData}
\alias{ImportData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
IMPORT EXPRERIMENT RESULTS
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ImportData(myfile, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{myfile}{
%%     ~~Describe \code{myfile} here~~
myfile is the name of a csv file, the file should currently under working directory
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}

\value{
%%  ~Describe the value returned
Return the content of the imported data
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{
%%  ~~who you are~~
Menghao Yu
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
ImportData("Test1.csv")
ImportData("Test2.csv")