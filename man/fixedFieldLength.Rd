\name{fixedFieldLength}
\alias{fixedFieldLength}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
EPOC Support function
}
\description{
Takes a value and return a character string with the optional width, decimal places and scientific notation style width.
}
\usage{
fixedFieldLength(value, width = "integer", sig = "integer", dec = "integer")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{value}{
Value to operate on.
}
  \item{width}{
Resulting string width
}
  \item{sig}{
Scientific notation width.
}
  \item{dec}{
Number of decimal places to round to.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Andrew Constable
}
\note{}
\seealso{
	\code{\link{EPOC}, \link{dayFromDate}, \link{getNoCase}, \link{asCSVCharacter}, \link{fromCSVCharacter}, \link{setEPOCCPPMethod}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
