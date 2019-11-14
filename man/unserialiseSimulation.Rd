\name{unserialiseSimulation}
\alias{unserialiseSimulation}
\title{
Load simulation/model from file.
}
\description{
Load a serialised simulation from file in cwd and have it ready to run.
}
\usage{
unserialiseSimulation(.Object, scenarioName)
}
\arguments{
  \item{.Object}{
Controller object
}
  \item{scenarioName}{
Name of scenario corresponding to filename in scenario directory.
}
}
\details{}
\value{
Returns controller object containing loaded and ready-to-go model.
}
\references{}
\author{
Troy Robertson
}
\note{}
\seealso{
	\code{\linkS4class{Controller}, \link{serialiseSimulation}}
}
\examples{
## Load serialised simulation from filename "scenario1"
# unserialiseSimulation(controller, "scenario1")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
