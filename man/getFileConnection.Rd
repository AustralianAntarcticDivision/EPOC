\name{getFileConnection}
\alias{getFileConnection}
\title{
EPOCObject file connection methods
}
\description{
Get the named open connection from the connections list if it exists.
If not and filepath is specified then attempt to open the connection, add it to the list
and return the open connection.
}
\usage{
getFileConnection(.Object, connname = "character", filepath = "character", openmode = "character")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{.Object}{
Element to act upon.
}
  \item{connname}{
Name of the connection.
}
  \item{filepath}{
Path to the file (optional if connection already exists)
}
  \item{openmode}{
Mode in which to open the file (defaults "a")
}
}
\details{
File connections are C++ class objects which are manipulated via the Rcpp package
interface.  EPOCObjects manage a list of externalptr(s) to these FileConnection objects.

If no connname exists in the fileConnections list and pathname is provided then a FileConnection
object will be instantiated with an open connection.  The externalptr will then be added to the list.

If a named list item exists then an attempt will still be made to open the connection if either the
connection is currently closed, or the pathname or openmode has changed (eg "w" to "r").
The fileConnections named list (and file connection API) is available from C++ and externalptrs can
then be created, used, closed from within C++ level Action methods.
}
\value{
Return an open file connection if successful, or NULL if not.
}
\references{}
\author{
Troy Robertson
}
\note{}
\seealso{
	\code{\linkS4class{EPOCObject}, \link{addFileConnection}, \link{closeFileConnection}, \link{writeFileConnection}, \link{readFileConnection}}
}
\examples{
## Get a new connection
# fc <- getFileConnection(element, "myNewConn", file.path("C:\\folder1", "MyFile"), "a")

## Get an existing connection
# fc <- getFileConnection(element, "myConn")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
