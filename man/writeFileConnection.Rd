\name{writeFileConnection}
\alias{writeFileConnection}
\title{
EPOCObject file connection methods
}
\description{
Write a line to the specified file connection and return success
}
\usage{
writeFileConnection(.Object, conn, ...)
}
\arguments{
  \item{.Object}{
EPOCObject
}
  \item{conn}{
Connection name
}
  \item{\dots}{
Additional character vectors to be concatenated using sep
}
  \item{filepath}{
Path to file to be opened if not already open
}
  \item{openmode}{
File mode (default = "a")
}
  \item{sep}{
Separator character (default = "")
}
  \item{eol}{
Append an end of line character (default = TRUE)
}
}
\details{
If a character name and filepath is passed the connection will be opened (if not open or path/mode differs)
and the connection will be added to the fileConnections list.
Do not attempt to pass a list as msg, or ... Please convert using asCSCharacter() first.
Method will write a single line to the file connection with an end of line character appended by default.
}
\value{
Return logical indicating success
}
\references{}
\author{
Troy Robertson
}
\note{}
\seealso{
	\code{\linkS4class{EPOCObject}, \link{getFileConnection}, \link{addFileConnection}, \link{closeFileConnection}, \link{readFileConnection}}
}
\examples{
## Append a character vector to the existing connection as a list
# success <- writeFileConnection(epocObject, "myConn", "Write this to file")

## Append a number of character vectors to a new connection
# writeFileConnection(epocObject, "myNewConn", "Just a", "line to", "append", filepath=file.path(getwd(), "to", "file"), sep=" ")

## Truncate and write again using a list
# writeFileConnection(epocObject, "myNewConn", asCSVCharacter(list("Another", "line", "yet", "again"), sep=" "), openmode="w")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
