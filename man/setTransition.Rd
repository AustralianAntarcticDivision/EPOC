\name{setTransition}
\alias{setTransition}
\title{
Element methods
}
\description{
Set transition item with value specified.\cr
Alternatively, replace the complete transition list with that passed.
}
\usage{
setTransition(.Object, item = c("character", "missing"), value)
}
\arguments{
  \item{.Object}{
Element object
}
  \item{item}{
List item in transition list to set with value
}
  \item{value}{
Value to set item (or transition) to
}
}
\details{}
\value{
Returns modified element object invisibly
}
\references{}
\author{
Troy Robertson
}
\note{}
\seealso{
	\code{\linkS4class{Element}, \linkS4class{EPOCObject}, \link{getTransition}}
}
\examples{
## Set transition value of foo to bar
# setTransition(element, item="foo", value="bar")

## Set transition list as trans
# setTransition(element, value=trans)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
