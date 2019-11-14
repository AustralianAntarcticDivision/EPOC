\name{serialiseSimulation}
\alias{serialiseSimulation}
\title{
Serialise current model to file.
}
\description{
Serialise and write "ready-to-go" simulation/model to file in cwd and named by scenario.
}
\usage{
serialiseSimulation(.Object, scenarioName)
}
\arguments{
  \item{.Object}{
Controller object
}
  \item{scenarioName}{
Name to save model as (eg scenarioName.epo)
}
}
\details{}
\value{}
\references{}
\author{
Troy Robertson
}
\note{}
\seealso{
	\code{\linkS4class{Controller}, \link{unserialiseSimulation}}
}
\examples{
## Write current simulation out to file named "file1"
# serialiseSimulation(controller, "scenario1")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
