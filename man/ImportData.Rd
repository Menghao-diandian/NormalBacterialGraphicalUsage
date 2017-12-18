\name{ImportData}

\alias{ImportData}
\alias{CalMean}
\alias{CalSD}
\alias{BarPlot}
\alias{GrowthCurve}

\title{
NORMAL BACTERIAL GRAPHICAL USAGE
}

\description{
  ImportData function import and check experiment data
  CalMean calculates mean of imported data
  CalSD calculates standard deviation of imported data
  BarPlot creates bar plot for normal phenotypic assays in several testing
  groups
  GrowthCurve creates line chart to show bacterial growth condition
}

\usage{
ImportData(myfile, rep, measurementPos, strainName, conditionName)
CalMean(myData, rep, measurementPos, StrainColName, conditionColName, 
strainName, conditionName)
CalSD(myData, rep, measurementPos, StrainColName, conditionColName, 
strainName, conditionName)
BarPlot(myData, rep, measurementPos, StrainColName, conditionColName,
strainName, conditionName, ylabName)
GrowthCurve(myData, rep, measurementPos, StrainColName, conditionColName,
strainName, timePoint)
}

\arguments{
  \item{myfile}{
myfile is the name of a csv file, refer to data(Test1) for proper format
}
  \item{rep}{
Replication number of each experiment setting, should be same among 
all settings
}
  \item{measurementPos}{
Number of the column which stores measurement
}
  \item{strainName}{
Name of tested strains selected to be plotted
}
  \item{conditionName}{
Tested conditions selected to be plotted
}
  \item{myData}{
Imported experiment data, refer to data(Test1) for proper format
}
  \item{StrainColName}{
In imported data, the name of the column which stores strain name
}
  \item{conditionColName}{
In imported data, the name of the column which stores testing condition
}
  \item{ylabName}{
Name of y-axis in exported bar plot
}
  \item{timePoint}{
Time point at which OD600 is measured
}
}

\value{
ImportData returns the content of the imported data
CalMean returns mean of each tested group in a matrix
CalSD returns standard deviation of each tested group in a matrix
BarPlot a bar plot indicating the difference of several bacterial 
strains in several conditions.
GrowthCurve a line chart indicating the growth condition of several 
bacterial strains. 
}

\author{
Menghao Yu
}

\examples{
mypath <- system.file ("extdata", "Test1.csv", 
                       package = "NormalBacterialGraphicalUsage")

a <- c("WT", "clpXP", "clpXP(pXP)") 
b <- c(24, 48)
ImportData (mypath, rep = 3, measurementPos = 3, 
            strainName = a, conditionName = b)

data(Test1)
CalMean (Test1, rep = 3, measurementPos = 3, 
         StrainColName = "Strain", 
         conditionColName = "Time", 
         strainName = a, conditionName = b)
         
CalSD (Test1, rep = 3, measurementPos = 3, 
       StrainColName = "Strain", 
       conditionColName = "Time", 
       strainName = a, conditionName = b)

BarPlot (Test1, rep = 3, measurementPos = 3, 
         StrainColName = "Strain", 
         conditionColName = "Time", 
         strainName = a, conditionName = b, 
         ylabName = "Distance (mm)")
         
data(Test2)
d <- c("WT(p)", "#3(p)", "#3(pT)")
e <- c(3, 6, 9, 12, 15, 18, 21, 24)
GrowthCurve (Test2, rep = 3, measurementPos = 3, 
             StrainColName = "Strain", 
             conditionColName = "Time",
             strainName = d, timePoint = e)
}