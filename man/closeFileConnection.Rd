\name{closeFileConnection}
\alias{closeFileConnection}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
EPOCObject file connection methods
}
\description{
Close named file connection if it is in list.
If conn is not specified then close all listed file connections.
}
\usage{
closeFileConnection(.Object, conn, ...)
}
\arguments{
  \item{.Object}{
EPOCObject
}
  \item{conn}{
List element name of the connection to close
}
}
\details{}
\value{
Returns EPOCObject.
}
\references{}
\author{
Troy Robertson
}
\note{}
\seealso{
	\code{\linkS4class{EPOCObject}, \link{getFileConnection}, \link{addFileConnection}, \link{writeFileConnection}, \link{readFileConnection}}
}
\examples{
## Close 'output' connection
# closeFileConnection(element, conn="output")
# closeFileConnection(element, conn=externalptr)

## Close all connection
# closeFileConnection(element)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
