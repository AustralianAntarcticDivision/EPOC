\name{EPOCObject-class}
\Rdversion{1.1}
\docType{class}
\alias{EPOCObject-class}
\alias{.logEPOCMessage,EPOCObject-method}
\alias{[[<-,EPOCObject-method}
\alias{addFileConnection,EPOCObject,externalptr,character-method}
\alias{closeFileConnection,EPOCObject,character-method}
\alias{closeFileConnection,EPOCObject,externalptr-method}
\alias{closeFileConnection,EPOCObject,missing-method}
\alias{epocDebugMessage,EPOCObject-method}
\alias{epocErrorMessage,EPOCObject-method}
\alias{epocMessage,EPOCObject-method}
\alias{epocVerboseMessage,EPOCObject-method}
\alias{getAttribute,EPOCObject,character-method}
\alias{getAttribute,EPOCObject,missing-method}
\alias{getAttributeNames,EPOCObject-method}
\alias{getFileConnection,EPOCObject,character-method}
\alias{getSignature,EPOCObject,character-method}
\alias{getSignature,EPOCObject,missing-method}
\alias{getSignatureLine,EPOCObject-method}
\alias{getSignatureMulti,EPOCObject-method}
\alias{getSlot,EPOCObject,character-method}
\alias{getSlotNames,EPOCObject-method}
\alias{initialize,EPOCObject-method}
\alias{parseInputData,EPOCObject-method}
\alias{readFileConnection,EPOCObject,character-method}
\alias{readFileConnection,EPOCObject,externalptr-method}
\alias{setAttribute,EPOCObject,character-method}
\alias{setSlot,EPOCObject,character-method}
\alias{writeFileConnection,EPOCObject,character-method}
\alias{writeFileConnection,EPOCObject,externalptr-method}

\title{Class "EPOCObject" - Base virtual EPOC object class from which others extend }
\description{
Base (virtual) class which all others inherit from.\cr
This includes the mechanisms for loading input data and parsing it into slots.\cr  
All input data which does not match a slot is inserted into the params slot list.\cr  
Its methods are available to all other class objects.\cr
This class inherits from .environment.\cr
Provides base data members and functionality including a signature slot.
}
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Slots}{
	 \describe{
    \item{\code{inputData}:}{Object of class \code{"list"} - Data list as imported from data input file. }
    \item{\code{dataPath}:}{Object of class \code{"character"} - Path at which data inputf ile is located. }
    \item{\code{signature}:}{Object of class \code{"Signature"} - Signature of object. }
    \item{\code{epocAttributes}:}{Object of class \code{"list"} - Attribute list for object }
    \item{\code{.xData}:}{Object of class \code{"environment"} - Environment data (to provide a pass-by-reference mechanism. }
  }
}
\section{Extends}{
Class \code{"\linkS4class{.environment}"}, directly.
Class \code{"\linkS4class{environment}"}, by class ".environment", distance 2, with explicit coerce.
Class \code{"\linkS4class{refObject}"}, by class ".environment", distance 3, with explicit coerce.
}
\section{Methods}{
  \describe{
	\item{\link[EPOC:addFileConnection]{addFileConnection}}{\code{signature(.Object = "EPOCObject", conn = "externalptr", connname = "character")}: ... }
    \item{\link[EPOC:closeFileConnection]{closeFileConnection}}{\code{signature(.Object = "EPOCObject", conn = c("character", "externalptr", "missing"))}: ... }
	\item{\link[EPOC:epocMessage]{epocDebugMessage}}{\code{signature(.Object = "EPOCObject")}: ... }
    \item{\link[EPOC:epocMessage]{epocErrorMessage}}{\code{signature(.Object = "EPOCObject")}: ... }
    \item{\link[EPOC:epocMessage]{epocMessage}}{\code{signature(.Object = "EPOCObject")}: ... }
    \item{\link[EPOC:epocMessage]{epocVerboseMessage}}{\code{signature(.Object = "EPOCObject")}: ... }
    \item{\link[EPOC:getAttribute]{getAttribute}}{\code{signature(.Object = "EPOCObject", item = c("character", "missing"))}: ... }
    \item{\link[EPOC:getAttributeNames]{getAttributeNames}}{\code{signature(.Object = "EPOCObject")}: ... }
    \item{\link[EPOC:getFileConnection]{getFileConnection}}{\code{signature(.Object = "EPOCObject", connname = "character")}: ... }
    \item{\link[EPOC:getSignature]{getSignature}}{\code{signature(.Object = "EPOCObject", item = c("character", "missing"))}: ... }
    \item{\link[EPOC:getSignatureLine]{getSignatureLine}}{\code{signature(.Object = "EPOCObject")}: ... }
    \item{\link[EPOC:getSignatureMulti]{getSignatureMulti}}{\code{signature(.Object = "EPOCObject")}: ... }
    \item{\link[EPOC:getSlot]{getSlot}}{\code{signature(.Object = "EPOCObject", item = "character")}: ... }
    \item{\link[EPOC:getSlotNames]{getSlotNames}}{\code{signature(.Object = "EPOCObject")}: ... }
    \item{\link[EPOC:parseInputData]{parseInputData}}{\code{signature(.Object = "EPOCObject")}: ... }
    \item{\link[EPOC:readFileConnection]{readFileConnection}}{\code{signature(.Object = "EPOCObject", conn = c("character", "externalptr"))}: ... }
    \item{\link[EPOC:setAttribute]{setAttribute}}{\code{signature(.Object = "EPOCObject", item = "character")}: ... }
    \item{\link[EPOC:setSlot]{setSlot}}{\code{signature(.Object = "EPOCObject", item = "character")}: ... }
    \item{\link[EPOC:setSlot]{writeFileConnection}}{\code{signature(.Object = "EPOCObject", conn = c("character", "externalptr"))}: ... }
     }
}
\references{ ~put references to the literature/web site here ~ }
\author{ Troy Robertson }
\note{}
\seealso{
	\code{\link{EPOC}, \linkS4class{Element}, \linkS4class{Signature}}
}
\examples{
showClass("EPOCObject")
}
\keyword{classes}
