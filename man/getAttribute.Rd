\name{getAttribute}
\alias{getAttribute}
\title{
EPOCObject method
}
\description{
Return the value of the attribute (item) specified from the epocAttributes slot.
}
\usage{
getAttribute(.Object, item = c("character", "missing"))
}
\arguments{
  \item{.Object}{
EPOCObject
}
  \item{item}{
Attribute name to be returned (epocAttributes list item)
}
}
\details{
An error will be thrown and execution stop if the item does not exist
The entire epocAttributes list will be returned if item=missing
}
\value{
Attribute value
}
\references{}
\author{ Troy Robertson }
\note{}
\seealso{
	\code{\linkS4class{EPOCObject}, \link{getAttributeNames}, \link{setAttribute}}
}
\examples{
## Return object attribute
# getAttribute(element, item="polygonsN")

## Return entire attributes list
# getAttribute(element)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
