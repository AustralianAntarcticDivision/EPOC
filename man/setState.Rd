\name{setState}
\alias{setState}
\title{
Element methods
}
\description{
Set element state list item with value, or alternatively set the entire state list.
}
\usage{
setState(.Object, item = c("character", "missing"), value)
}
\arguments{
  \item{.Object}{
Element object
}
  \item{item}{
State list item to be set with value (optional)
}
  \item{value}{
Value to be set at item
}
}
\details{}
\value{
Returns modified element object invisibly
}
\references{}
\author{
Troy Robertson
}
\note{}
\seealso{
	\code{\linkS4class{Element}, \linkS4class{EPOCObject}, \link{getState}}
}
\examples{
## Set the state list item "mortality" to 666
# setState(element, item="mortality", value=666)

## Set entire state list
# setState(element, value=stateList)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
