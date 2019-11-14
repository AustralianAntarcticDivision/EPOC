\name{getNoCase}
\alias{getNoCase}
\title{
EPOC Support function
}
\description{
Return a list element based on a case insensitive name.
Return NULL if not in list.
}
\usage{
getNoCase(x, element = "character")
}
\arguments{
  \item{x}{
List
}
  \item{element}{
element name
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
	\code{\link{EPOC}, \link{dayFromDate}, \link{fixedFieldLength}, \link{asCSVCharacter}, \link{fromCSVCharacter}, \link{setEPOCCPPMethod}}
}
\examples{
## Return named list element
alist <- list(a=1, b=2, c=3)
getNoCase(alist, "A")

# Returns NULL
getNoCase(alist, "D")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
