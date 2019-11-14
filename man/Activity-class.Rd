\name{Activity-class}
\Rdversion{1.1}
\docType{class}
\alias{Activity-class}
\alias{initialize,Activity-method}

\title{Class "Activity" - Virtual element class.}
\description{Virtual class to be extended by all elements of this module type.}
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Slots}{
	 \describe{
    \item{\code{polygons}:}{Object of class \code{"list"} ~~ }
    \item{\code{polygonsN}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{birthday}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{timesteps}:}{Object of class \code{"list"} ~~ }
    \item{\code{timestepsN}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{recordElements}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{currentScenarioDir}:}{Object of class \code{"character"} ~~ }
    \item{\code{inputData}:}{Object of class \code{"list"} ~~ }
    \item{\code{dataPath}:}{Object of class \code{"character"} ~~ }
    \item{\code{signature}:}{Object of class \code{"Signature"} ~~ }
    \item{\code{epocAttributes}:}{Object of class \code{"list"} ~~ }
    \item{\code{.xData}:}{Object of class \code{"environment"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{Element}"}, directly.
Class \code{"\linkS4class{EPOCObject}"}, by class "Element", distance 2.
Class \code{"\linkS4class{.environment}"}, by class "Element", distance 3.
Class \code{"\linkS4class{environment}"}, by class "Element", distance 4, with explicit coerce.
Class \code{"\linkS4class{refObject}"}, by class "Element", distance 5, with explicit coerce.
}
\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "Activity")}: ... }
	 }
}
\references{ ~put references to the literature/web site here ~ }
\author{ Troy Robertson, Andrew Constable }
\note{}
\seealso{
	\code{\linkS4class{EPOCObject}, \linkS4class{Signature}, \linkS4class{Biota}, \linkS4class{Environment}, \linkS4class{Management}, \linkS4class{Output}, \linkS4class{Presentation}}
}
\examples{
showClass("Activity")
}
\keyword{classes}
