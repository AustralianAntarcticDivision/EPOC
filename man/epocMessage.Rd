\name{epocMessage}
\alias{epocMessage}
\alias{epocVerboseMessage}
\alias{epocDebugMessage}
\alias{epocErrorMessage}
\title{
EPOC messaging
}
\description{
Message that is written to stdout if msglevel output is set appropriately.
msglevel settings can be one of c("quiet", "normal", "verbose", "debug") 
Alternatively setting R option for "epocmsglevel" will take precedence over
msglevel argument to Universe or Controller constructors.
Otherwise level is taken from Universe data input values or fallback default to "normal"
}
\usage{
epocMessage(msg = "character", ..., sep="")
epocVerboseMessage(msg = "character", ..., sep="")
epocDebugMessage(msg = "character", ..., sep="")
epocErrorMessage(msg = "character", ..., sep="", halt=FALSE)
}
\arguments{
\item{.Object}{
EPOCObject
}
  \item{msg}{
Character string to print to stdout
}
  \item{\dots}{
Any additional strings to append.
}
\item{sep}{
Separator character (default="")
}
\item{halt}{
Use R stop() rather than message() (default=FALSE)
}
}
\details{}
\value{}
\references{}
\author{ Troy Robertson }
\note{}
\seealso{
	\code{\link{EPOC}, \link[EPOC:epocMessage]{epocDebugMessage}, \link[EPOC:epocMessage]{epocVerboseMessage}}
}
\examples{
## This global flags are normally set up when controller is instantiated.
## msglevel = "normal"
## Print a standard EPOC message
#epocMessage(epocobj, "This will be ", "printed to stdout.")

## The silent flag will kill any output though
## msglevel = "quiet"
# epocVerboseMessage(epocobj, "But this won't be printed to stdout")

## Print error message then stop
# epocErrorMessage(epocobj, "Bad error!", halt=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
