\name{getSlot}
\alias{getSlot}
\title{
EPOCObject methods
}
\description{
Return data at the object slot specified.
}
\usage{
getSlot(.Object, item = "character")
}
\arguments{
  \item{.Object}{
EPOCObject
}
  \item{item}{
Slot name to return from
}
}
\details{
Note that .xData will be checked if item is not a slot
Will return NA if item = "" and will stop execution with an error if item does not exist as a slot
}
\value{
Value at slot item
}
\references{}
\author{ 
Troy Robertson 
}
\note{}
\seealso{
	\code{\linkS4class{EPOCObject}, \link{getSlotNames}, \link{setSlot}}
}
\examples{
## Return object slot
# getSlot(epocObject, item="foobar")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
