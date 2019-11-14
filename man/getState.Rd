\name{getState}
\alias{getState}
\title{
Element methods
}
\description{
Return the state list or item in the state list for this element object.
}
\usage{
getState(.Object, item = c("character", "missing"))
}
\arguments{
  \item{.Object}{
Element object
}
  \item{item}{
State list item to return (optional)
}
}
\details{}
\value{
Value at item in state list
}
\references{}
\author{
Troy Robertson
}
\note{}
\seealso{
	\code{\linkS4class{Element}, \linkS4class{EPOCObject}, \link{setState}}
}
\examples{
## Return the item "mortality" in the state list
# getState(element, item="mortality")

## Return the entire state list
# getState(element)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
