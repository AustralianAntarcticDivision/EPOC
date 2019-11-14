\name{getScenario}
\alias{getScenario}
\title{
Universe methods
}
\description{
Return current scenario object (or specific scenario name or number, or slot item in specific scenario) for universe.
}
\usage{
getScenario(.Object, scenario = c("character", "numeric", "missing"), item = c("character", "missing"))
}
\arguments{
  \item{.Object}{
Universe object
}
  \item{scenario}{
Scenario name or scenario number (optional)
}
  \item{item}{
Scenario slot item (optional)
}
}
\details{}
\value{
Return scenario object.
}
\references{}
\author{
Troy Robertson
}
\note{}
\seealso{
	\code{\linkS4class{Scenario}, \linkS4class{Universe}, \linkS4class{EPOCObject}, \link{getReport}, \link{getSpatial}, \link{getRTState}}
}
\examples{
## Return the current scenario
# getTrial(universe)

## Return trial1
# getTrial(universe, scenario="scenario1")

## Return startYear attribute from scenario1
# getTrial(universe, scenario="scenario1", item="startYear")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
