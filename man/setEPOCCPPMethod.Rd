\name{setEPOCCPPMethod}
\alias{setEPOCCPPMethod}
\title{
Support methods
}
\description{
Provides a shortcut function for setting a method for an element class.  
Compiles body using package 'inline'
Sets a generic method (if needed) and an EPOC Action method for class element
Rip offs of inline functions setCMethod() which was failing to pass on ... args
in its internal cfunction() call, and also adds default compiler flags for EPOC 
}
\usage{
setEPOCCPPMethod(method, element, body, ...)
}
\arguments{
  \item{method}{
Method name
}
  \item{element}{
Element name
}
  \item{body}{
C++ code
}
  \item{\dots}{
Additional args
}
}
\details{}
\value{}
\references{}
\author{
Credit inline & Rcpp package teams
}
\note{}
\seealso{
	\code{\link{EPOC}, \linkS4class{Element}, \link{setEPOCLibMethod}, \link{cfunction}}
}
\examples{
## To create a cpp EPOC Action method
# code<-"
# epocVerboseMessage(universe, \"Doing stuff...\");
# epocVerboseMessage(universe, getSignatureMulti(sexpObj));
# "
# setEPOCCPPMethod("doStuff", "Krill", body=code)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
