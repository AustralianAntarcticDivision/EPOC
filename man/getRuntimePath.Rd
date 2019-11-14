\name{getRuntimePath}
\alias{getRuntimePath}
\title{
Universe methods
}
\description{
Return the path to the current scenarios runtime output directory with extPath appended if passed.
}
\usage{
getRuntimePath(.Object, extPath)
}
\arguments{
  \item{.Object}{
Universe object
}
  \item{extPath}{
optional path extension
}
}
\details{}
\value{
String file path as massaged for platform by file.path()
}
\references{}
\author{
Troy Robertson
}
\note{}
\seealso{
	\code{\linkS4class{Universe}, \linkS4class{EPOCObject}, \link{getBasePath}}
}
\examples{
## Return path
# getRuntimePath(universe, "B.MI.Es.KPFM.state.R")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
