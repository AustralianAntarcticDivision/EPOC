\name{outputCalendar}
\alias{outputCalendar}
\title{
EPOC Controller methods
}
\description{
Create a textual output from universe calendar object and write to file.\cr
Optionally write calendar to screen as well.
}
\usage{
outputCalendar(.Object, universe = "Universe", tofile = "logical")
}
\arguments{
  \item{.Object}{
Controller object
}
  \item{universe}{
Universe object
}
  \item{tofile}{
Write to file (default=TRUE)
}
}
\details{}
\value{}
\references{}
\author{
Andrew Constable, Troy Robertson
}
\note{}
\seealso{
	\code{\linkS4class{Controller}, \linkS4class{Universe}, \linkS4class{Calendar}, \link{runSimulation}, \link{serialiseSimulation}, \link{unserialiseSimulation}}
}
\examples{
## Write to file only
# outputCalendar(calendar)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
