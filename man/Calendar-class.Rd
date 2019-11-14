\name{Calendar-class}
\Rdversion{1.1}
\docType{class}
\alias{Calendar-class}
\alias{.convertTSToElementPeriods,Calendar,Universe-method}
\alias{.generateActionMatrix,Calendar,Universe-method}
\alias{.getElementActionMat,Calendar,Element,numeric,numeric-method}
\alias{.initialiseCalendar,Calendar,Universe-method}
\alias{.setupPeriodTS,Calendar,Universe-method}
\alias{createCalendar,Calendar,Universe-method}
\alias{getActionMatForPeriod,Calendar,numeric-method}
\alias{getInfoForPeriod,Calendar,numeric-method}
\alias{getPeriod,Calendar,numeric-method}
\alias{printCalendar,Calendar,Universe-method}

\title{Class "Calendar" - Calendar of scenario actions}
\description{
Establishes and represents a calendar of actions for the current scenario.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Calendar", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{periods}:}{Object of class \code{"list"} ~~ }
    \item{\code{periodN}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{.xData}:}{Object of class \code{"environment"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{.environment}"}, directly.
Class \code{"\linkS4class{environment}"}, by class ".environment", distance 2, with explicit coerce.
Class \code{"\linkS4class{refObject}"}, by class ".environment", distance 3, with explicit coerce.
}
\section{Methods}{
  \describe{
	\item{.convertTSToElementPeriods}{\code{signature(.Object = "Calendar", universe = "Universe")}: ... }
    \item{.generateActionMatrix}{\code{signature(.Object = "Calendar", universe = "Universe")}: ... }
    \item{.getElementActionMat}{\code{signature(.Object = "Calendar", element = "Element", modulenum = "numeric", elementnum = "numeric")}: ... }
    \item{.initialiseCalendar}{\code{signature(.Object = "Calendar", universe = "Universe")}: ... }
    \item{.setupPeriodTS}{\code{signature(.Object = "Calendar", universe = "Universe")}: ... }
    \item{\link[EPOC:createCalendar]{createCalendar}}{\code{signature(.Object = "Calendar")}: ... }
    \item{\link[EPOC:getActionMatForPeriod]{getActionMatForPeriod}}{\code{signature(.Object = "Calendar")}: ... }
    \item{\link[EPOC:getInfoForPeriod]{getInfoForPeriod}}{\code{signature(.Object = "Calendar")}: ... }
    \item{\link[EPOC:getPeriod]{getPeriod}}{\code{signature(.Object = "Calendar")}: ... }
    \item{\link[EPOC:printCalendar]{printCalendar}}{\code{signature(.Object = "Calendar", universe = "Universe")}: ... }
	 }
}
\references{ ~put references to the literature/web site here ~ }
\author{Andrew Constable, Troy Robertson}
\note{}
\seealso{
	\code{\link{EPOC}, \linkS4class{Controller}, \linkS4class{Universe}, \linkS4class{Period}, \linkS4class{EPOCObject}}
}
\examples{
showClass("Calendar")
}
\keyword{classes}
