\name{doFlag}
\alias{doFlag}
\alias{doUpdate}
\alias{doPrint}
\alias{doPrintFinal}
\title{
EPOC Element runtime flags
}
\description{
Set or get runtime flags for the EPOC Element specified
}
\usage{
## doFlag is the generic method
doFlag(.Object, flag="character", do=c("logical", "missing"))
doUpdate(.Object, do=c("logical", "missing"))
doPrint(.Object, do=c("logical", "missing"))
doPrintFinal(.Object, do=c("logical", "missing"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{.Object}{
EPOC Element
}
  \item{flag}{
name of flag to set/get
}
  \item{do}{
optional logical value for setting of flag
}
}
\details{
doFlag is the higher level method called by the other wrappers.
User can create/set/get their own runtime flags using this method.
doUpdate is used to specify whether an updateState method will be called for the period
doPrint is used to specify whether a printState method will be called for the period
doPrintFinal is used to specify whether a printState method will be called at the
end of the scenario
}
\value{
return value of flag (invisibly if setter was called)
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Troy Robertson
}
\note{
}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Set doPrint flag
# doPrint(element, TRUE)
## Get doPrint flag
# dp <- doPrint(element)
## Set users runtime flag
# doFlag(element, "MyFlag", TRUE)
## Get doPrint flag
# df <- doFlag(element, "MyFlag")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
