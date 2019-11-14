\name{getTimestep}
\alias{getTimestep}
\title{
Element methods
}
\description{
Return timestep list (or item for period) for this element object.
}
\usage{
getTimestep(.Object, periodNum = "numeric")
}
\arguments{
  \item{.Object}{
Element object
}
  \item{periodNum}{
Period number
}
}
\details{}
\value{
Timestep value in list
}
\references{}
\author{
Troy Robertson
}
\note{}
\seealso{
	\code{\linkS4class{Element}, \linkS4class{EPOCObject}, \link{setTimestep}}
}
\examples{
##
# getTimestep(element, periodNum=3)

## Return the entire timestep list
# getTimestep(element)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
