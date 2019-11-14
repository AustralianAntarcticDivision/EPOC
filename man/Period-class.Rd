\name{Period-class}
\Rdversion{1.1}
\docType{class}
\alias{Period-class}
\alias{getPeriodActionMat,Period-method}
\alias{getPeriodElementTSData,Period,numeric,numeric-method}
\alias{getPeriodInfo,Period-method}
\alias{initialize,Period-method}

\title{Class "Period" - Object representing a single period }
\description{Period object holds information about the period, including
a period action matrix.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Period", universe, periodPropYear, periodKEData, previousDay, periodNumber)}.
}
\section{Slots}{
	 \describe{
    \item{\code{number}:}{Object of class \code{"numeric"} - Period number. }
    \item{\code{day}:}{Object of class \code{"numeric"} - Number of days in period. }
    \item{\code{knifeEdge}:}{Object of class \code{"logical"} - Is this a knife edge action? }
    \item{\code{yearPropn}:}{Object of class \code{"numeric"} - Proportion of year consumed by action. }
    \item{\code{periodStart}:}{Object of class \code{"numeric"} - Starting Julian day of year. }
    \item{\code{periodEnd}:}{Object of class \code{"numeric"} - Ending Julian day of year. }
    \item{\code{modules}:}{Object of class \code{"list"} - List of modules/elements/periodInfo. }
    \item{\code{periodActionMat}:}{Object of class \code{"list"} - Action matrix for period. }
  }
}
\section{Methods}{
  \describe{
    \item{getPeriodActionMat}{\code{signature(.Object = "Period")}: ... }
    \item{getPeriodElementTSData}{\code{signature(.Object = "Period", modnum = "numeric", elemnum = "numeric")}: ... }
    \item{getPeriodInfo}{\code{signature(.Object = "Period")}: ... }
    \item{initialize}{\code{signature(.Object = "Period")}: ... }
	 }
}
\references{}
\author{ Troy Robertson, Andrew Constable }
\note{}
\seealso{
	\code{\link{EPOC}, \linkS4class{Calendar}, \linkS4class{EPOCObject}}
}
\examples{
showClass("Period")
}
\keyword{classes}
