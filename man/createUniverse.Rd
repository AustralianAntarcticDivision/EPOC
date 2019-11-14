\name{createUniverse}
\alias{createUniverse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
createUniverse
}
\description{
Load and create Universe from input data files
}
\usage{
createUniverse(.Object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{.Object}{
Universe object
}
}
\details{
Build the universe object and its module elements based on the universe.data input file
# that was passed at instantiation
}
\value{
}
\references{
}
\author{
Troy Robertson
}
\note{
}

\seealso{
\code{\link{EPOC}, \linkS4class{Universe}}
}
\examples{
## Instantiate universe
# universe <- new("Universe", dataPath="C:\Blah\uni.data.R")
## Create universe from input data
# createUniverse(universe)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
