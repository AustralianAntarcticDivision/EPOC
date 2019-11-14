\name{getTransition}
\alias{getTransition}
\title{
Element methods
}
\description{
Return transition list (or item in list) for this element object.
}
\usage{
getTransition(.Object, item = c("character", "missing"))
}
\arguments{
  \item{.Object}{
Element object
}
  \item{item}{
Item in transition list to be returned (optional)
}
}
\details{}
\value{
Value at item in transition list
}
\references{}
\author{
Troy Robertson
}
\note{}
\seealso{
	\code{\linkS4class{Element}, \linkS4class{EPOCObject}, \link{setTransition}}
}
\examples{
## Return transition list item "consumption"
# getTransition(element, item="consumption")

## Return entire transition list
# getTransition(element)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
