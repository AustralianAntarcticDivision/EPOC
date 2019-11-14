\name{getSignatureMulti}
\alias{getSignatureMulti}
\title{
EPOCObject method
}
\description{
Return a multi-line textual representation of the EPOCObject signature
}
\usage{
getSignatureMulti(.Object)
}
\arguments{
  \item{.Object}{
EPOCObject
}
\item{display}{
Send signature to stdout as epocMessage (default=FALSE)
}
}
\details{
}
\value{
Multiline string representation of signature
}
\references{
}
\author{
Troy Robertson
}
\note{
}
\seealso{
	\code{\linkS4class{EPOCObject}, \link{getSignature}, \link{getSignatureLine}}
}
\examples{
## Get signature and send to stdout as epocMessage
# sm <- getSignatureMulti(obj, TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
