\name{getReport}
\alias{getReport}
\title{
Universe methods
}
\description{
Return reporting attribute list (or specific list item in report list) for universe.
}
\usage{
getReport(.Object, item = c("character", "missing"))
}
\arguments{
  \item{.Object}{
Universe object
}
  \item{item}{
Report list item to return (optional)
}
}
\details{}
\value{
Report value
}
\references{}
\author{
Troy Robertson
}
\note{}
\seealso{
	\code{\linkS4class{Universe}, \linkS4class{EPOCObject}, \link{getRTState}, \link{getSpatial}, \link{getScenario}}
}
\examples{
## Get report list
# getReport(universe)

## Get reporting diagnostics
# getReport(universe, item="Diagnostics")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
