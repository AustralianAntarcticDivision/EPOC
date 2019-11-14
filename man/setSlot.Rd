\name{setSlot}
\alias{setSlot}
\title{
EPOCObject methods
}
\description{
Set object slot item with value
}
\usage{
setSlot(.Object, item = "character", value)
}
\arguments{
  \item{.Object}{
EPOCObject
}
  \item{item}{
Name of slot
}
  \item{value}{
Value to be assigned to slot
}
}
\details{
Note: Will check .xData for object with same name if slot not found
}
\value{}
\references{}
\author{ Troy Robertson }
\note{}
\seealso{
	\code{\linkS4class{EPOCObject}, \link{getSlotNames}, \link{setSlot}}
}
\examples{
## Set slot "foo" to "bar"
# setSlot(element, item="foo", value="bar")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
