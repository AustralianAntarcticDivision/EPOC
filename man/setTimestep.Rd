\name{setTimestep}
\alias{setTimestep}
\title{
Element methods
}
\description{
Set element timestep list for period index specified.\cr
Alternatively, set the entire timestep list for this element.
}
\usage{
setTimestep(.Object, periodNum = c("numeric", "missing"), value)
}
\arguments{
  \item{.Object}{
Element object
}
  \item{periodNum}{
Period number (optional if list passed as value)
}
  \item{value}{
Timestep value to set at periodNum
}
}
\details{}
\value{
Returns modified element object.
}
\references{}
\author{
Troy Robertson
}
\note{}
\seealso{
	\code{\linkS4class{Element}, \linkS4class{EPOCObject}, \link{getTimestep}}
}
\examples{
## Set timestep for period 3 to ts
# setTimestep(element, periodNum=3, value=ts)

## Set the entire timestep list
# setTimestep(element, value=tsList)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
