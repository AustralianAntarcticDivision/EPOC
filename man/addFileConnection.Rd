\name{addFileConnection}
\alias{addFileConnection}
\title{
EPOCObject file connection methods
}
\description{
Add the external pointer to file connection to the fileConnections named list using connname
If connname already exists and is open then it will be closed and replaced.
}
\usage{
addFileConnection(.Object, conn, connname)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{.Object}{
EPOCObject
}
  \item{conn}{
FileConnection externalptr
}
  \item{connname}{
Name for connection
}
}
\details{
Will first close connection on any pre-existing connection of same connname
}
\value{
Returns EPOCObject
}
\references{
}
\author{
Troy Robertson
}
\note{
}
\seealso{
	\code{\linkS4class{EPOCObject}, \link{getFileConnection}, \link{closeFileConnection}, \link{writeFileConnection}, \link{readFileConnection}}
}
\examples{
## Add an existing connection
# addFileConnection(element, fc, "myNewConn")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
