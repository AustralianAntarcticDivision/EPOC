\name{Controller-class}
\Rdversion{1.1}
\docType{class}
\alias{Controller-class}
\alias{initialize,Controller-method}
\alias{closeDatafiles,Controller-method}
\alias{getCalendar,Controller-method}
\alias{getUniverse,Controller-method}
\alias{outputCalendar,Controller-method}
\alias{runSimulation,Controller-method}
\alias{serialiseSimulation,Controller-method}
\alias{unserialiseSimulation,Controller-method}

\title{Class "Controller" - EPOC controller engine}
\description{	Main EPOC modelling engine class.\cr
				This class controls the building of a calendar for the universe it is acting upon.\cr
				It is also responsible for the running of the simulation.}
\section{Objects from the Class}{
Objects can be created by calls of the form: \code{controller <- new("Controller", universe=universe, notify=getOptions('epocnotify'))}
}
\section{Slots}{
	 \describe{
    \item{\code{universe}:}{Object of class \code{"Universe"} - Model universe to control }
    \item{\code{calendar}:}{Object of class \code{"Calendar"} - Calendar of actions for the scenario }
    \item{\code{inputData}:}{Object of class \code{"list"} - Input data as read from input file at dataPath }
    \item{\code{dataPath}:}{Object of class \code{"character"} - Directory path to the input file }
    \item{\code{signature}:}{Object of class \code{"Signature"} - Signature describing this object }
    \item{\code{epocAttributes}:}{Object of class \code{"list"} - Attributes held by this object }
	\item{\code{epocdebug}:}{Object of class \code{"list"} - Debug constraints for simulation }
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
    \item{.projectPeriod}{\code{signature(.Object = "Controller")}: ... }
    \item{\link[EPOC:closeDatafiles]{closeDatafiles}}{\code{signature(.Object = "Controller")}: ... }
    \item{\link[EPOC:outputCalendar]{outputCalendar}}{\code{signature(.Object = "Controller")}: ... }
 	\item{closeDatafiles}{\code{signature(.Object = "Controller")}: ... }
    \item{getCalendar}{\code{signature(.Object = "Controller")}: ... }
    \item{getUniverse}{\code{signature(.Object = "Controller")}: ... }
    \item{initialize}{\code{signature(.Object = "Controller")}: ... }
    \item{\link[EPOC:unserialiseSimulation]{unserialiseSimulation}}{\code{signature(.Object = "Controller")}: ... }
    \item{\link[EPOC:runSimulation]{runSimulation}}{\code{signature(.Object = "Controller")}: ... }
    \item{\link[EPOC:serialiseSimulation]{serialiseSimulation}}{\code{signature(.Object = "Controller")}: ... }
	 }
}
\references{  }
\author{ Troy Robertson, Andrew Constable }
\note{ }
\seealso{
	\code{\link{EPOC}, \link{epoc}, \linkS4class{Universe}, \linkS4class{Calendar}, \linkS4class{EPOCObject}}
}
\examples{
showClass("Controller")

## First instantiate a new default demo universe
# universe <- new("Universe")

## Create a new controller object
# controller <- new("Controller", universe=universe)

## OR to change message level notification
# options(epocmsglevel="quiet", epocloglevel="debug")
# OR levels c("quiet", "normal", "verbose", "debug")
## OR pass to constructor
# controller <- new("Controller", universe=universe, epocmsglevel="quiet", epocloglevel="debug")

## Once processed the model can be serialised to file with:
# serialiseSimulation(controller, "scenario1", includeClassesAndMethods=TRUE)
## And a serialised simulation loaded by:
# controller <- new("Controller")
# controller <- unserialiseSimulation(controller, "scenario1")

## Write calendar to file (or screen)
# outputCalendar(controller, tofile=TRUE)

## The simulation can then be run with:
# runSimulation(controller, timer=TRUE)
}
\keyword{classes}
