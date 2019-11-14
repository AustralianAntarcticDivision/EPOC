\name{printCalendar}
\alias{printCalendar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Write calendar to file, optionally write to stdout instead
}
\description{
Output a textual representation of the calendar.  Defaults to file but with
tofile=FALSE will output to stdout
}
\usage{
printCalendar(.Object, universe = "Universe", tofile = "logical")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{.Object}{
Controller object.
}
  \item{universe}{
Universe object.
}
  \item{tofile}{
Do you want the calendar also printed to file? Defaults to TRUE
}
}
\details{}
\value{}
\references{}
\author{
Troy Robertson
}
\note{}
\seealso{
	\code{\linkS4class{Controller}}
}
\examples{
## Just write to file
# printCalendar(controller, universe)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
