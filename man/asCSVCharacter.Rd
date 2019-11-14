\name{asCSVCharacter}
\alias{asCSVCharacter}
\alias{fromCSVCharacter}
\title{
EPOC Support functions
}
\description{
asCSVCharacter returns a character string containing values passed separated by sep
fromCSVCharacter returns a vector (or list if list passed) of values split by sep
The type argument will specify the vector type to try cast to
}
\usage{
asCSVCharacter(values, ..., sep=";")
fromCSVCharacter(values, ..., type="double", sep=";")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{values}{
Values to separate (vector, list, matrix)
}
  \item{\dots}{
Additional values (in the case of asCSVCharacter)
}
  \item{sep}{
Separator character(s).  Default = ","
}
}
\details{
}
\value{
Single string (character vector) for asCSVCharacter and vector for fromCSVCharacter unless a list
is passed.
}
\references{
}
\author{
Troy Robertson
}
\note{
}
\seealso{
}
\examples{
# asCSVCharacter(list("foo", "bar", "you"), sep="-")
# fromCSVCharacter("foo,bar,you")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
