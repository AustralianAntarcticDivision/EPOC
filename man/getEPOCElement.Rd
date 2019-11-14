\name{getEPOCElement}
\alias{getEPOCElement}
\title{
Universe methods
}
\description{
Return the Element object specified by module and element list indexes
}
\usage{
getEPOCElement(.Object, modID, elemID)
}
\arguments{
  \item{.Object}{
Universe object
}
  \item{modID}{
Relative index of module in universe$modules list
}
  \item{elemID}{
Relative index of element in universe$modules[[modID]] module list
}
}
\details{}
\value{
Returns Element object or NULL if not found
}
\references{}
\author{
Troy Robertson
}
\note{}
\seealso{
	\code{\linkS4class{Universe}, \linkS4class{EPOCObject}, \link{setEPOCElement}}
}
\examples{
## Return 3rd element object in Biota module
# ele <- getEPOCElement(universe, 1, 3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
