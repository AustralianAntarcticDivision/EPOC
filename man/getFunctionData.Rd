\name{getFunctionData}
\alias{getFunctionData}
\title{
Element methods
}
\description{
Return element object function data list, or, specified item in list.
}
\usage{
getFunctionData(.Object, item = "character")
}
\arguments{
  \item{.Object}{
Element object
}
  \item{item}{
Optional element$functions list item
}
}
\details{}
\value{
Value of element .xData functions item
}
\references{}
\author{
Troy Robertson
}
\note{}
\seealso{
	\code{\linkS4class{Element}, \linkS4class{EPOCObject}, \link{setFunctionData}}
}
\examples{
## Return item in function data list
# getFunctionData(element, item="printState")

## Return entire function data list
# getFunctionData(element)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
