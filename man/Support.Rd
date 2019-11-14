\name{Support-functions}
\alias{Support-functions}
\title{EPOC Support functions}
\description{A collection of helper functions for use with EPOC}
\usage{
dayFromDate(day="numeric", month="numeric")
fixedFieldLength(value=c("numeric", "logical", "character"), width="integer", sig="integer", dec="integer")
getNoCase(x="list", element="character")
asCSVCharacter(values=c("vector", "list", "matrix"), ..., sep="character")
fromCSVCharacter(values=c("character", "list"), type="character", sep="character")
setEPOCCPPMethod(method="character", element="character", body="character", ...)
setEPOCLibMethod(method="character", element="character", libpath="character", libfn=method)
}
\references{}
\author{ Troy Robertson, Andrew Constable }
\note{ }
\seealso{
	\code{\link{EPOC}, \link{dayFromDate}, \link{fixedFieldLength}, \link{getNoCase}, \link{asCSVCharacter}, \link{fromCSVCharacter}, \link{setEPOCCPPMethod}, \link{setEPOCLibMethod}}
}
\examples{}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
