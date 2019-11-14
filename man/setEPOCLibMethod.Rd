\name{setEPOCLibMethod}
\alias{setEPOCLibMethod}
\title{
Support methods
}
\description{
Provides a shortcut function for setting an action method for an element class.  
Used to create an EPOC Action wrapper method around a .Call to libfn in shared library at libpath
Sets a generic (if needed) and method for class element.
}
\usage{
setEPOCLibMethod(method="character", element="character", libpath="character", libfn("character", "missing"))
}
\arguments{
  \item{method}{
Method name
}
  \item{element}{
Element name
}
  \item{libpath}{
File path to shared library
}
  \item{libfn}{
Name of function within library (defaults to method)
}
}
\details{}
\value{}
\references{}
\author{
Credit inline & Rcpp team
}
\note{}
\seealso{
	\code{\link{EPOC}, \linkS4class{Element}, \link{setEPOCCPPMethod}}
}
\examples{
## To create an EPOC Action method wrapper named 'doStuff' for function 'doit'
## in package 'myEPOCFunctions'
# setEPOCLibMethod("doStuff", "Krill", file.path(getwd(), "myEPOCFunctions"), "doit")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
