\name{setSignature}
\alias{setSignature}
\title{
EPOC Signature methods
}
\description{
Set the signature of an EPOCObject from the list supplied
}
\usage{
setSignature(.Object, signatureList = "list")
}
\arguments{
  \item{.Object}{
Signature object
}
  \item{signatureList}{
Named list matching slot names of signature objject
}
}
\details{}
\value{
Returns modified Signature object
}
\references{}
\author{ Troy Robertson }
\note{}
\seealso{
	\code{\linkS4class{EPOCObject}, \linkS4class{Signature}, \link{getSignatureItem}, \link{getFullSignature}, \link{getSimpleSignature}}
}
\examples{
## create and set signature
# sig <- new("Signature")
# sigList <- list(
# 	ClassName       = "Krill",
# 	ID              = 22001,
#	Name.full       = "Euphausia superba (Antarctic krill)",
#	Name.short      = "Es-KPFM",
#	Morph        	= "KPFM",
#	Revision      	= "01",
#	Authors      	= "A.Constable",
#	Last.edit    	= "7 July 2008"
# )
# setSignature(sig, sigList)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
