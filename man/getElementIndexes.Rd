\name{getListIndexesFromNames}
\alias{getListIndexesFromNames}
\title{
Universe methods
}
\description{
Return vector of module and element list indexes for the names passed,
or for the module name and EPOC Element ID passed.
}
\usage{
getListIndexesFromNames(.Object, moduleName = moduleName, element = elementName)
getListIndexesFromNames(.Object, moduleName = moduleName, element = elementID)
}
\arguments{
  \item{.Object}{
Universe object
}
  \item{moduleName}{
Name of the module that the element is within (optional)
}
  \item{element}{
Either the name or ID of the element to find
}
}
\details{
Returns indexes of 0 where not found (eg unknown module and element returns c(0,0)
}
\value{
Numeric vector as c(modindex, elemindex)
}
\references{}
\author{
Troy Robertson
}
\note{}
\seealso{
	\code{\linkS4class{Universe}, \linkS4class{EPOCObject}}
}
\examples{
## Return indexes for Krill element
# getListIndexesFromNames(universe, "Biota", "Krill")

## Return indexes for unknown module and element ID
# getListIndexesFromNames(universe, element=20004)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
