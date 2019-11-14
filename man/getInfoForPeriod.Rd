\name{getInfoForPeriod}
\alias{getInfoForPeriod}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calendar method
}
\description{
Return period information as a list for the period number specified.
}
\usage{
getInfoForPeriod(.Object, periodNum)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{.Object}{
Calendar object
}
  \item{periodNum}{
Period number
}
}
\details{}
\value{
Return list containing period information
}
\references{}
\author{
Troy Robertson
}
\note{}
\seealso{
	\code{\linkS4class{Calendar}, \linkS4class{Period}, \link{getPeriod}, \link{getActionMatForPeriod}, \link{createCalendar}, \link{printCalendar}}
}
\examples{
## Return period information for period 2
# getInfoForPeriod(calendar, 2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
