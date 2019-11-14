\name{setEPOCElement}
\alias{setEPOCElement}
\title{
Universe methods
}
\description{
Add an element object to universe at module modID and element elemID list indexes
}
\usage{
setEPOCElement(.Object, modID, elemID, element)
}
\arguments{
  \item{.Object}{
Universe object
}
  \item{modID}{
Relative index in universe$modules list
}
  \item{elemID}{
Relative index in module list
}
  \item{element}{
Element object to be set
}
}
\details{}
\value{
Return modified universe object invisibly
}
\references{}
\author{
Troy Robertson
}
\note{}
\seealso{
	\code{\linkS4class{Universe}, \linkS4class{EPOCObject}, \link{getEPOCElement}}
}
\examples{
## Add third element ele to universe in Biota module
# setEPOCElement(universe, 1, 3, ele)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
