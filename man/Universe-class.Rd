\name{Universe-class}
\Rdversion{1.1}
\docType{class}
\alias{Universe-class}

\alias{createCalendar,Calendar,Universe-method}
\alias{createUniverse,Universe-method}
\alias{getBasePath,Universe,character-method}
\alias{getBasePath,Universe,missing-method}
\alias{getElementIndexes,Universe,character,character-method}
\alias{getElementIndexes,Universe,character,numeric-method}
\alias{getElementIndexes,Universe,missing,character-method}
\alias{getElementIndexes,Universe,missing,numeric-method}
\alias{getEPOCElement,Universe-method}
\alias{getReport,Universe,character-method}
\alias{getReport,Universe,missing-method}
\alias{getRTState,Universe,character-method}
\alias{getRTState,Universe,missing-method}
\alias{getRuntimePath,Universe,character-method}
\alias{getRuntimePath,Universe,missing-method}
\alias{getSpatial,Universe,character-method}
\alias{getSpatial,Universe,missing-method}
\alias{getScenario,Universe,ANY,character-method}
\alias{getScenario,Universe,ANY,missing-method}
\alias{getScenario,Universe,missing,missing-method}
\alias{initialiseElementTransitions,Scenario,Universe-method}
\alias{initialiseElementScenarios,Scenario,Universe-method}
\alias{initialiseTransition,Fishery,Universe-method}
\alias{initialiseTransition,Krill,Universe-method}
\alias{initialiseTransition,Predator,Universe-method}
\alias{initialiseReplicate,Fishery,Universe-method}
\alias{initialiseReplicate,Krill,Universe-method}
\alias{initialiseReplicate,KrillEnvironment,Universe-method}
\alias{initialiseReplicate,Manager,Universe-method}
\alias{initialiseReplicate,Predator,Universe-method}
\alias{initialize,Universe-method}
\alias{printCalendar,Calendar,Universe-method}
\alias{regionalSeries,KrillEnvironment,Universe-method}
\alias{reSourceElementClasses,Universe-method}
\alias{reSourceElementMethods,Universe-method}
\alias{setEPOCElement,Universe,ANY,ANY,Element-method}
\alias{testInputPaths,Universe-method}

\title{Class "Universe" - Model universe object class }
\description{
Object which represents and defines the model universe which is to be simulated. }
\section{Objects from the Class}{
Objects can be created by calls of the form: \code{universe <- new("Universe", dataPath=file.path(getwd(), "data", "Universe.data.R"), notify=getOption("epocnotify"), create=TRUE)}.
}
\section{Slots}{
	 \describe{
    \item{\code{spatial}:}{Object of class \code{"Spatial"} - Spatial object for universe. }
    \item{\code{scenarios}:}{Object of class \code{"list"} - Scenarios list for simulation. }
    \item{\code{report}:}{Object of class \code{"list"} - Reporting characteristics. }
    \item{\code{inputPaths}:}{Object of class \code{"list"} - List of member element input data paths. }
	\item{\code{baseDirectory}:}{Object of class \code{"character"} - Path to input base directory. }
    \item{\code{inputData}:}{Object of class \code{"list"} - Raw input data. }
    \item{\code{dataPath}:}{Object of class \code{"character"} - Path to input data file. }
	\item{\code{created}:}{Object of class \code{"logical"} - Has universe been loaded and built. }
    \item{\code{signature}:}{Object of class \code{"Signature"} - Signature object. }
    \item{\code{epocAttributes}:}{Object of class \code{"list"} - Object attributes. }
    \item{\code{.xData}:}{Object of class \code{"environment"} - Contains realtimeState and modules lists. }
  }
}
\section{Extends}{
Class \code{"\linkS4class{EPOCObject}"}, directly.
Class \code{"\linkS4class{.environment}"}, by class "EPOCObject", distance 2.
Class \code{"\linkS4class{environment}"}, by class "EPOCObject", distance 3, with explicit coerce.
Class \code{"\linkS4class{refObject}"}, by class "EPOCObject", distance 4, with explicit coerce.
}
\section{Methods}{
  \describe{
    \item{\link[EPOC:createUniverse]{createUniverse}}{\code{signature(.Object = "Universe")}: ... }
	\item{\link[EPOC:createCalendar]{createCalendar}}{\code{signature(.Object = "Calendar", universe = "Universe")}: ... }
    \item{\link[EPOC:getEPOCElement]{getEPOCElement}}{\code{signature(.Object = "Universe")}: ... }
    \item{\link[EPOC:getElementIndexes]{getElementIndexes}}{\code{signature(.Object = "Universe")}: ... }
	\item{\link[EPOC:getBasePath]{getBasePath}}{\code{signature(.Object = "Universe", extPath = c("character", "missing")))}: ... }
    \item{\link[EPOC:getRuntimePath]{getRuntimePath}}{\code{signature(.Object = "Universe", extPath = c("character", "missing"))}: ... }
    \item{\link[EPOC:getElementIndexes]{getElementIndexes}}{\code{signature(.Object = "Universe", moduleName = c("character", "missing"), element = c("character", "numeric"))}: ... }
    \item{\link[EPOC:getReport]{getReport}}{\code{signature(.Object = "Universe", item = c("character", "missing"))}: ... }
    \item{\link[EPOC:getRTState]{getRTState}}{\code{signature(.Object = "Universe", item = c("character", "missing"))}: ... }
    \item{\link[EPOC:getSpatial]{getSpatial}}{\code{signature(.Object = "Universe", item = c("character", "missing"))}: ... }
    \item{\link[EPOC:getScenario]{getScenario}}{\code{signature(.Object = "Universe", scenario = c("character", "numeric", "missing"), item = c("character", "missing"))}: ... }
	\item{\link[EPOC:initialiseElementTransitions]{initialiseElementTransitions}}{\code{signature(.Object = "Scenario", universe = "Universe")}: ... }
    \item{\link[EPOC:initialiseElementScenarios]{initialiseElementScenarios}}{\code{signature(.Object = "Scenario", universe = "Universe")}: ... }
    \item{\link[EPOC:initialiseTransition]{initialiseTransition}}{\code{signature(.Object = "Element", universe = "Universe")}: ... }
    \item{\link[EPOC:initialiseReplicate]{initialiseReplicate}}{\code{signature(.Object = "Element", universe = "Universe")}: ... } 
	\item{\link[EPOC:printCalendar]{printCalendar}}{\code{signature(.Object = "Calendar", universe = "Universe")}: ... }
    \item{\link[EPOC:setEPOCElement]{setEPOCElement}}{\code{signature(.Object = "Universe", modID = "ANY", elemID = "ANY", element = "Element")}: ... }
    \item{testInputPaths}{\code{signature(.Object = "Universe")}: ... }
	 }
}
\references{}
\author{ Troy Robertson, Andrew Constable }
\note{}
\seealso{
	\code{\link{EPOC}, \link{epoc}, \linkS4class{Controller}, \linkS4class{Calendar}, \linkS4class{Scenario}, \linkS4class{Element}}
}
\examples{
showClass("Universe")

## Create a new universe object using data input file specified
# universe <- new("Universe", dataPath=file.path(getwd(), "data", "Universe.data.R"))

## Instantiate but do not load universe elements yet
# universe <- new("Universe", dataPath=file.path(getwd(), "data", "Universe.data.R"), create=FALSE)

## OR to change notification level notification
# options(epocmsglevel="quiet")
## OR to change logging level
# options(epocloglevel="debug")
# Available levels c("quiet", "normal", "verbose", "debug")
## OR pass to constructor
# universe <- new("Universe", dataPath=file.path(getwd(), "data", "Universe.data.R"), msglevel="quiet", loglevel="debug")

}
\keyword{classes}
