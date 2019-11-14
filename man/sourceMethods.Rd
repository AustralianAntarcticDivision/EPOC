\name{sourceMethods}
\alias{sourceMethods}
\title{
Element methods
}
\description{
source() all element action methods as per that elements data input file
}
\usage{
sourceMethods(.Object, basePath = "character")
}
\arguments{
  \item{.Object}{
Element object
}
  \item{basePath}{
Base directory from which code directory can be found
}
}
\details{}
\value{
A vector of full path names to all action methods
}
\references{}
\author{ Troy Robertson }
\note{}
\seealso{
	\code{\linkS4class{Element}, \linkS4class{EPOCObject}}
}
\examples{
# sourceMethods(element, basePath="C:\foo\bar")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
