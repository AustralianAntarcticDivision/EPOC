\name{Spatial-class}
\Rdversion{1.1}
\docType{class}
\alias{Spatial-class}
\alias{initialize,Spatial-method}

\title{Class "Spatial" - Universe spatial object class }
\description{Class representing a universes spatial characterisitics.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Spatial", dataPath)}.
}
\section{Slots}{
	 \describe{
    \item{\code{polygonNames}:}{Object of class \code{"character"} - List of polygon names. }
    \item{\code{polygonAreas}:}{Object of class \code{"numeric"} - List of polygon areas. }
    \item{\code{coords}:}{Object of class \code{"list"} - List of coordinates. }
    \item{\code{coordAreas}:}{Object of class \code{"list"} - List of coordinate areas. }
    \item{\code{coordProportions}:}{Object of class \code{"list"} - List of proportions for each polygon. }
    \item{\code{overlap}:}{Object of class \code{"matrix"} - Boolean matrix indicating adjacency. }
    \item{\code{polygonN}:}{Object of class \code{"numeric"} - Total count of polygons. }
    \item{\code{inputData}:}{Object of class \code{"list"} - Raw input data from input data file. }
    \item{\code{dataPath}:}{Object of class \code{"character"} - File path at which data input file is found. }
    \item{\code{signature}:}{Object of class \code{"Signature"} - Signature object. }
    \item{\code{epocAttributes}:}{Object of class \code{"list"} - Object attribute list. }
    \item{\code{.xData}:}{Object of class \code{"environment"} - Environment data. }
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
    \item{initialize}{\code{signature(.Object = "Spatial")}: ... }
	 }
}
\references{}
\author{ Troy Robertson }
\note{}
\seealso{
	\code{\link{EPOC}, \linkS4class{EPOCObject}, \linkS4class{Signature}}
}
\examples{
showClass("Spatial")
}
\keyword{classes}
