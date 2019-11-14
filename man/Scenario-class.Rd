\name{Scenario-class}
\Rdversion{1.1}
\docType{class}
\alias{Scenario-class}
\alias{initialiseElementTransitions,Scenario, Universe-method}
\alias{initialiseElementScenarios,Scenario, Universe-method}
\alias{initialize,Scenario-method}

\title{Class "Scenario" - Defines the bounds of a single simulation }
\description{Represents scenario parameters for a single simulation of universe model}
\section{Objects from the Class}{
Objects can be created by calls of the form: \code{new("Scenario", universe, dataPath, scenarioNum)}.
}
\section{Slots}{
	 \describe{
    \item{\code{scenarioNum}:}{Object of class \code{"numeric"} - Scenario number (index). }
    \item{\code{yearStart}:}{Object of class \code{"numeric"} - Starting Julian day of year. }
    \item{\code{yearEnd}:}{Object of class \code{"numeric"} - Ending Julian day of year. }
    \item{\code{yearsN}:}{Object of class \code{"numeric"} - Number of years. }
    \item{\code{firstFishingYear}:}{Object of class \code{"numeric"} - First fishing Julian day of year. }
    \item{\code{lastFishingYear}:}{Object of class \code{"numeric"} - Last fishing Julian day of year. }
    \item{\code{scenarioDir}:}{Object of class \code{"character"} - Path to scenario directory. }
    \item{\code{inputData}:}{Object of class \code{"list"} - Raw input data from input data file. }
    \item{\code{dataPath}:}{Object of class \code{"character"} - Path to input data file. }
    \item{\code{signature}:}{Object of class \code{"Signature"} - Signature object. }
    \item{\code{epocAttributes}:}{Object of class \code{"list"} - Object attributes. }
    \item{\code{.xData}:}{Object of class \code{"environment"} }
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
    \item{\link[EPOC:initialiseElementTransitions]{initialiseElementTransitions}}{\code{signature(.Object = "Scenario")}: ... }
    \item{\link[EPOC:initialiseElementScenarios]{initialiseElementScenarios}}{\code{signature(.Object = "Scenario")}: ... }
    \item{initialize}{\code{signature(.Object = "Scenario")}: ... }
	 }
}
\references{}
\author{ Troy Robertson, Andrew Constable }
\note{}
\seealso{
	\code{\link{EPOC}, \linkS4class{Controller}, \linkS4class{Calendar}, \linkS4class{Universe}, \linkS4class{Element}}
}
\examples{
showClass("Scenario")
}
\keyword{classes}
