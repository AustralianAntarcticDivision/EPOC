\name{getSpatial}
\alias{getSpatial}
\title{
Universe methods
}
\description{
Return the Spatial object (or specific slot item in spatial object) for universe
}
\usage{
getSpatial(.Object, item = c("character", "missing"))
}
\arguments{
  \item{.Object}{
Universe object
}
  \item{item}{
Spatial slot name
}
}
\details{}
\value{
Value at item in spatial slot or entire spatial object
}
\references{}
\author{
Troy Robertson
}
\note{}
\seealso{
	\code{\linkS4class{Spatial}, \linkS4class{Universe}, \linkS4class{EPOCObject}, \link{getReport}, \link{getRTState}, \link{getScenario}}
}
\examples{
## Return spatial object for universe
# getSpatial(universe)

## Return polygon count
# getSpatial(universe, item="polygonN")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
