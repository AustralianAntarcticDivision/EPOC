\name{setAttribute}
\alias{setAttribute}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
EPOCObject methods
}
\description{
Set an element attribute in its list with value.
}
\usage{
setAttribute(.Object, item = c("character", "missing"), value=c("ANY", "list"))
}
\arguments{
  \item{.Object}{
EPOCObject
}
  \item{item}{
Name of EPOC attribute to insert or overwrite
}
  \item{value}{
Value to insert or overwrite as
}
}
\details{}
\value{
EPOCObject passed
}
\references{}
\author{ Troy Robertson }
\note{}
\seealso{
	\code{\linkS4class{EPOCObject}, \link{getAttributeNames}, \link{getAttribute}}
}
\examples{
## Set attribute foo with value "bar"
# setAttribute(element, item="foo", value="bar")

## Set all EPOC attributes with a list
# setAttribute(element, value=list("foo"="bar", "bar"="foo"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
