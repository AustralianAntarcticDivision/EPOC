\name{getBasePath}
\alias{getBasePath}
\title{
Universe methods
}
\description{
Return the path to the base input directory with extPath appended if passed.
}
\usage{
getInputPath(.Object, extPath = "character")
}
\arguments{
  \item{.Object}{
Universe object
}
  \item{extPath}{
Optional path extension to base
}
}
\details{}
\value{
String file path as massaged for platform by file.path()
}
\references{}
\author{ Troy Robertson }
\note{}
\seealso{
	\code{\linkS4class{Universe}, \linkS4class{EPOCObject}, \link{getRuntimePath}}
}
\examples{
## Return path
# getBasePath(universe, "data\B.MI.Es.KPFM.data.01.R")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
