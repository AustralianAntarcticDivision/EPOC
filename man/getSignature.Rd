\name{getSignature}
\alias{getSignature}
\title{
EPOCObject method
}
\description{
Return the Signature object for this EPOC object, or a slot from the signature object.
}
\usage{
getSignature(.Object, item = "character")
}
\arguments{
  \item{.Object}{
EPOCObject object
}
  \item{item}{
period slot item (optional)
}
}
\details{}
\value{
Period object if item missing, otherwise contents of item slot
}
\references{}
\author{ Troy Robertson }
\note{}
\seealso{
	\code{\linkS4class{EPOCObject}, \link{getSignatureLine}, \link{getSignatureMulti}}
}
\examples{
## Return signature object
# getSignature(element)

## Return Name.full
# getSignature(element, item="Name.full")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
