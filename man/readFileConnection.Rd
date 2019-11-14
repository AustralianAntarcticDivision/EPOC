\name{readFileConnection}
\alias{readFileConnection}
\title{
EPOCObject file connection methods
}
\description{
Read all lines as list (or linenum line as character vector) from connection, opening connection if
necessary and pathname is provided.  Will change file mode from "a" or "w" to "r" if necessary.
}
\usage{
readFileConnection(.Object, conn, ...)
}
\arguments{
  \item{.Object}{
EPOCObject
}
  \item{conn}{
Connection name
}
  \item{linenum}{
Particular line number in file to be read starting at line 1.  If 0 is passed then next line will
be read dependent on the C++ file pointer.  Default is -1 which will return a list containing all
lines of the text file.
}
  \item{filepath}{
Path to file for which a connection will be created
}
  \item{openmode}{
File connection open mode (default = "r")
}
}
\details{

}
\value{
Returns a list corresponding to each line in the file if no linenum is specified.
Otherwise return a character vector holding the line text with no eol character.
}
\references{}
\author{
Troy Robertson
}
\note{}
\seealso{
	\code{\linkS4class{EPOCObject}, \link{getFileConnection}, \link{addFileConnection}, \link{closeFileConnection}, \link{writeFileConnection}}
}
\examples{
## Read in the contents of the existing connection as a list
# lines <- readFileConnection(epocObject, "myConn")

## Read in line 5 from new connection
# line5 <- readFileConnection(epocObject, "myNewConn", linenum=5, filepath=file.path(getwd(), "to", "file"), openmode="r")

## Read in next line (line 6)
# line6 <- readFileConnection(epocObject, "myNewConn", linenum=0)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
