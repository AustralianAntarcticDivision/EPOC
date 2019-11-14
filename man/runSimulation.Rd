\name{runSimulation}
\alias{runSimulation}
\title{
EPOC Controller methods
}
\description{
Main method to start a run once universe is loaded.
}
\usage{
runSimulation(.Object, ...)
}
\arguments{
  \item{.Object}{
Controller object.
}
  \item{timer}{
Output run time details after completion (default=FALSE)
}
  \item{epocdebug}{
Runtime break targets for simulation.
One or more of c("pre_action", "post_action", "period", "year", "scenario")
or can break at particular value(s) c(action="migrate", year=1950)
}
  \item{forceGC}{
Should garbage  collection be forced before commencing simulation?
}
}
\details{}
\value{}
\references{}
\author{ 
Troy Robertson 
}
\seealso{
	\code{\link{EPOC}, \linkS4class{Controller}, \linkS4class{Universe}, \linkS4class{Calendar}, \link{runSimulation}, \link{serialiseSimulation}, \link{unserialiseSimulation}}
}
\examples{
## Run simulation but do garbage collection first
# runSimulation(controller, forceGC=TRUE)

## Run simulation and output timing
# runSimulation(controller, timing=TRUE)

## Run simulation and break after every updateState method
# runSimulation(controller, epoc_debug=c(post_action="updateState"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
