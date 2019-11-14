\name{EPOC-package}
\alias{EPOC-package}
\alias{EPOC}
\docType{package}
\title{
Ecosystem Productivity Ocean Climate (EPOC) - Modelling Framework
}
\description{
An object-oriented modelling framework built around a number of modules: Environment, Biota, Activities, Management, Output and Presentation
}
\details{
\tabular{ll}{
Package: \tab EPOC\cr
Type: \tab Package\cr
Version: \tab 0.5.0\cr
Date: \tab 2012-05-01\cr
License: \tab GPL2\cr
Depends: \tab methods, Rcpp\cr
Suggests: \tab inline\cr
}
Input data files are used to define and structure a model universe including it's spatial definition, reporting and scenario parameters.\cr
A model universe will generally also include one or more elements of type: Environment, Biota, Activities, Management, Output and Presentation.\cr
Each element may also be defined by one or more attributes and actions.
Scenario parameters and element Actions determine how the model will run.
}
\author{
Andrew Constable\cr
Troy Robertson

Maintainer: Troy Robertson <troy.robertson@aad.gov.au>
}
\references{}
\keyword{ package }
\seealso{
\code{\link{epoc}, \linkS4class{Controller}, \linkS4class{Universe}, \linkS4class{Calendar} , \linkS4class{Element}, \linkS4class{EPOCObject}, \link{Support-functions}}
}
\examples{
## Set the root working directory for input/output files
# setwd(...)

## The EPOC package contains a set of example input data files and action methods. These can
## be run simply by instantiating the convenience function epoc().  This function will load input files
## create a model universe and run the simulation just by:
# e <- epoc()


## Alternatively a user created universe will be run by:
# e <- epoc(dataPath=file.path(getwd(), "data", "Universe.data.R"))


## OR to skip the convenience.
## Create a model universe using passed input data file
# universe <- new("Universe", dataPath=file.path(getwd(), "data", "Universe.data.R"))

## Start controller - this creates/sets up both the universe and calendar
# controller <- new("Controller", universe=universe, outputcalendar=TRUE)

## Start simulation
# runSimulation(controller, timer=TRUE)
}
