\name{getSignatureItem}
\alias{getSignatureItem}
\title{
EPOC Signature methods
}
\description{
Return signature component with the specified name.
}
\usage{
getSignatureItem(.Object, item = "character")
}
\arguments{
  \item{.Object}{
Signature object
}
  \item{item}{
Signature slot item to return
}
}
\details{}
\value{
Signature slot item
}
\references{}
\author{ 
Troy Robertson 
}
\note{}
\seealso{
	\code{\linkS4class{EPOCObject}, \linkS4class{Signature}, \link{getSimpleSignature}, \link{getFullSignature}, \link{setSignature}}
}
\examples{
## Return Authors
# getSignatureItem(sig, item="Authors")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
