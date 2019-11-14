\name{Element-class}
\Rdversion{1.1}
\docType{class}
\alias{Element-class}
\alias{closeFileConnection,Element-method}
\alias{doFlag,Element,character,logical-method}
\alias{doFlag,Element,character,missing-method}
\alias{doPrint,Element,logical-method}
\alias{doPrint,Element,missing-method}
\alias{doPrintFinal,Element,logical-method}
\alias{doPrintFinal,Element,missing-method}
\alias{doUpdate,Element,logical-method}
\alias{doUpdate,Element,missing-method}
\alias{getBirthday,Element-method}
\alias{getFunctionData,Element,character-method}
\alias{getFunctionData,Element,missing-method}
\alias{getPolygons,Element-method}
\alias{getState,Element,character-method}
\alias{getState,Element,missing-method}
\alias{getTimestep,Element,missing-method}
\alias{getTimestep,Element,numeric-method}
\alias{getTransition,Element,character-method}
\alias{getTransition,Element,missing-method}
\alias{initialize,Element-method}
\alias{setEPOCElement,Universe,ANY,ANY,Element-method}
\alias{setFunctionData,Element,character,ANY-method}
\alias{setFunctionData,Element,missing,list-method}
\alias{setState,Element,character,ANY-method}
\alias{setState,Element,missing,list-method}
\alias{setTimestep,Element,missing,list-method}
\alias{setTimestep,Element,numeric,ANY-method}
\alias{setTransition,Element,character,ANY-method}
\alias{setTransition,Element,missing,list-method}
\alias{sourceMethods,Element-method}

\title{Class "Element" - Active universe component.}
\description{
Represents an active Universe component.\cr
Will be sub-classed into one of 5 module types:\cr
- Biota\cr
- Environment\cr
- Activity\cr
- Management\cr
- Output\cr
- Presentation\cr
These module classes are then sub-classed to a direct EPOC element class e.g. Krill which may then be instantiated through the use
of input data files for both the universe object and the element object to be instantiated.}
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Slots}{
  \describe{
	\item{\code{polygons}:}{Object of class \code{"vector"} - Vector of polygon indexes in which this element is found.}
    \item{\code{polygonsN}:}{Object of class \code{"numeric"}  - Total number of polygons found in. }
    \item{\code{birthday}:}{Object of class \code{"numeric"}  - Day of the year defined as start of cycle. }
    \item{\code{timesteps}:}{Object of class \code{"list"}  - List of timesteps defined by actions. }
    \item{\code{timestepsN}:}{Object of class \code{"numeric"} - Total number of timesteps. }
    \item{\code{recordElements}:}{Object of class \code{"numeric"}  -  }
    \item{\code{currentScenarioDir}:}{Object of class \code{"character"}  - Output directory for active scenario. }
    \item{\code{inputData}:}{Object of class \code{"list"} - Input data as sourced from input data file. }
    \item{\code{dataPath}:}{Object of class \code{"character"} - Path to input data file. }
    \item{\code{signature}:}{Object of class \code{"Signature"} - Signature definition for element. }
    \item{\code{epocAttributes}:}{Object of class \code{"list"} - Element attributes pulled from input data. }
    \item{\code{.xData}:}{Object of class \code{"environment"} - Environment list containing non-static Element data. }
  }
}
\section{Extends}{
Class \code{"\linkS4class{EPOCObject}"}, directly.
Class \code{"\linkS4class{.environment}"}, by class "EPOCObject", distance 2.
Class \code{"\linkS4class{environment}"}, by class "EPOCObject", distance 3, with explicit coerce.
}
\section{Methods}{
  \describe{
	\item{\link[EPOC:doFlag]{doFlag}}{\code{signature(.Object = "Element", flag = "character", do = c("logical", "mising"))}: ... }
    \item{\link[EPOC:doFlag]{doPrint}}{\code{signature(.Object = "Element", do = c("logical", "mising"))}: ... }
    \item{\link[EPOC:doFlag]{doPrintFinal}}{\code{signature(.Object = "Element", do = c("logical", "mising"))}: ... }
	\item{\link[EPOC:doFlag]{doUpdate}}{\code{signature(.Object = "Element", do = c("logical", "mising"))}: ... }
	\item{\link[EPOC:getBirthday]{getBirthday}}{\code{signature(.Object = "Element")}: ... }
	\item{\link[EPOC:getFileConnection]{getFileConnection}}{\code{signature(.Object = "Element")}: ... }
    \item{\link[EPOC:getFunctionData]{getFunctionData}}{\code{signature(.Object = "Element", item = c("character", "missing"))}: ... }
	\item{\link[EPOC:getPolygons]{getPolygons}}{\code{signature(.Object = "Element")}: ... }
    \item{\link[EPOC:getState]{getState}}{\code{signature(.Object = "Element", item = c("character", "missing"))}: ... }
    \item{\link[EPOC:getTimestep]{getTimestep}}{\code{signature(.Object = "Element", periodNum = c("numeric", "missing"))}: ... }
    \item{\link[EPOC:getTransition]{getTransition}}{\code{signature(.Object = "Element", item = c("character", "missing"))}: ... }
    \item{initialize}{\code{signature(.Object = "Element")}: ... }
    \item{\link[EPOC:setFunctionData]{setFunctionData}}{\code{signature(.Object = "Element", item = c("character", "missing"), value = "ANY")}: ... }
    \item{\link[EPOC:setState]{setState}}{\code{signature(.Object = "Element", item = c("character", "missing"), value = "ANY")}: ... }
    \item{\link[EPOC:setTimestep]{setTimestep}}{\code{signature(.Object = "Element", periodNum = c("numeric", "missing"), value = "ANY")}: ... }
    \item{\link[EPOC:setTransition]{setTransition}}{\code{signature(.Object = "Element", item = c("character", "missing"), value = "ANY")}: ... }
    \item{\link[EPOC:sourceMethods]{sourceMethods}}{\code{signature(.Object = "Element")}: ... }
	 }
}
\references{}
\author{ Troy Robertson, Andrew Constable }
\note{}
\seealso{
	\code{\linkS4class{EPOCObject}, \linkS4class{Signature}, \linkS4class{Biota}, \linkS4class{Environment}, \linkS4class{Activity}, \linkS4class{Management}, \linkS4class{Output}, \linkS4class{Presentation}}
}
\examples{
showClass("Element")
}
\keyword{classes}
