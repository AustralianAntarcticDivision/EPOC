\name{Signature-class}
\Rdversion{1.1}
\docType{class}
\alias{Signature-class}
\alias{getFullSignature,Signature-method}
\alias{getSimpleSignature,Signature-method}
\alias{getSignatureItem,Signature-method}
\alias{setSignature,Signature-method,list-method}

\title{Class "Signature" - EPOCObject signature object }
\description{Object represents a signature with its data members}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Signature", ...)}.
}
\section{Slots}{
	 \describe{
    \item{\code{ClassName}:}{Object of class \code{"character"} - Name of class to implement. }
    \item{\code{ID}:}{Object of class \code{"numeric"} - EPOC ID of object. }
    \item{\code{ID.absolute}:}{Object of class \code{"numeric"} - Same as above. }
    \item{\code{Name.full}:}{Object of class \code{"character"} - Full name of object. }
    \item{\code{Name.short}:}{Object of class \code{"character"} - A shortened name suitable for files etc. }
    \item{\code{Morph}:}{Object of class \code{"character"} - Morphology of object. }
    \item{\code{Revision}:}{Object of class \code{"character"} - Revision/version of object. }
    \item{\code{Authors}:}{Object of class \code{"character"} - Authors names. }
    \item{\code{Last.edit}:}{Object of class \code{"character"} - Date of last modification. }
  }
}
\section{Methods}{
  \describe{
    \item{\link[EPOC:getFullSignature]{getFullSignature}}{\code{signature(.Object = "Signature")}: ... }
    \item{\link[EPOC:getSimpleSignature]{getSimpleSignature}}{\code{signature(.Object = "Signature")}: ... }
    \item{\link[EPOC:getSignatureItem]{getSignatureItem}}{\code{signature(.Object = "Signature")}: ... }
    \item{\link[EPOC:setSignature]{setSignature}}{\code{signature(.Object = "Signature", signatureList = "list")}: ... }
	 }
}
\references{ ~put references to the literature/web site here ~ }
\author{ Troy Robertson }
\note{}
\seealso{
	\code{\link{EPOC}, \linkS4class{EPOCObject}, \linkS4class{Element}}
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

## Output full signature
# epocMessage(element, getFullSignature(sig))
}
\keyword{classes}
