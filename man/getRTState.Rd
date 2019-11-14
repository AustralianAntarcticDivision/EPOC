\name{getRTState}
\alias{getRTState}
\title{
Universe methods
}
\description{
Return real-time state list (or specific list item in real-time state) for universe.
}
\usage{
getRTState(.Object, item = c("character", "missing"))
}
\arguments{
  \item{.Object}{
Universe object
}
  \item{item}{
Realtime state list item to return (optional)
}
}
\details{}
\value{
Value held in item or entire realtime state list
}
\references{}
\author{
Troy Robertson
}
\note{}
\seealso{
	\code{\linkS4class{Universe}, \linkS4class{EPOCObject}, \link{getReport}, \link{getSpatial}, \link{getScenario}}
}
\examples{
## Return RTState list
# getRTState(universe)

## Return CurrentYear
# getRTState(universe, item="CurrentYear")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
