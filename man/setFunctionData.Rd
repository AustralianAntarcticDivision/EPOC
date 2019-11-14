\name{setFunctionData}
\alias{setFunctionData}
\title{
Element methods
}
\description{
Set additonal user function data for element object./cr
Set either the entire list or a single list item.
}
\usage{
setFunctionData(.Object, item = c("character", "missing"), value)
}
\arguments{
  \item{.Object}{
Element object
}
  \item{item}{
List item in functions list to set value at.  Optional if value="list"
}
  \item{value}{
Value to be set
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
	\code{\linkS4class{Element}, \linkS4class{EPOCObject}, \link{getFunctionData}}
}
\examples{
## Set function data item "printState" to TRUE
# setFunctionData(element, item="printState", value=TRUE)

## Set entire function data list
# setFunctionData(element, value=fdlist)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
