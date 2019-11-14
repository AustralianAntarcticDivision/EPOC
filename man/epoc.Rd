\name{epoc}
\alias{epoc}
\title{epoc() - Convenience function.
}
\description{
Convenience function allowing simple execution of an EPOC universe model.
By default, this will run the package example universe, unless the cwd is the
root of an EPOC directory and contains a 'data' directory which in turn contains
a 'Universe.data.R' input data file.
A dataPath to such a file can also be passed to the function.
}
\usage{
epoc(dataPath=NULL, outputcalendar=TRUE, tofile=TRUE, msglevel="normal", 
				loglevel="verbose", logfile="EPOCDemo.log", logtrunc=TRUE, timer=TRUE)
}
\arguments{
  \item{dataPath}{ - Full path to an EPOC universe data input file.}
  \item{outputcalendar}{ - Should the universe timestep calendar be displayed?}
  \item{tofile}{ - If calendar is to be produced, should it be to file or to stdout?}
  \item{timer}{ - Should the elapsed run time for the simulation be displayed?}
  \item{msglevel}{ - Explicit EPOC message level to stdout c("quiet", "normal", "verbose", "debug")}
  \item{loglevel}{ - Explicit EPOC logging level to loogfile c("quiet", "normal", "verbose", "debug")}
  \item{logfile}{ - Name of log file (default = "EPOC.log")}
  \item{logtrunc}{ - Sould logfile be truncated before writintg (default = TRUE)}
}
\details{}
\value{Returns the EPOC Controller object created.}
\references{}
\author{ Troy Robertson }
\note{}
\seealso{
	\code{\link{EPOC}, \linkS4class{Universe}, \linkS4class{Calendar}, \linkS4class{EPOCObject}}
}
\examples{
## Set the root working directory for input/output files
# setwd(...)

## The EPOC package contains a set of example input data files and action methods. These can
## be run simply by instantiating the convenience function epoc().  This function will load input files
## create a model universe and run the simulation just by:
# con <- epoc()


## Alternatively a user created universe will be run by:
# con <- epoc(dataPath=file.path(getwd(), "data", "Universe.data.R"))


## OR to skip the convenience.
## Create a model universe using passed input data file
# universe <- new("Universe", dataPath=file.path(getwd(), "data", "Universe.data.R"))

## Start controller - this creates/sets up both the universe and calendar
# controller <- new("Controller", universe=universe, outputcalendar=TRUE, tofile=TRUE)

## Start simulation
# runSimulation(controller)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
