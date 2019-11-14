\name{getPeriodElementTSData}
\alias{getPeriodElementTSData}
\title{
Period method
}
\description{
Return the timestep data for the element specified for this period object
}
\usage{
getPeriodElementTSData(.Object, modnum, elemnum)
}
\arguments{
  \item{.Object}{
Period object
}
  \item{modnum}{
Relative module number
}
  \item{elemnum}{
Relative element number
}
}
\details{}
\value{
Matrix of period timesteps of 3 columns:
[,1] = Timestep number
[,2] = Timesteps % of period					
[,3] = Periodstep number within timestep
}
\references{}
\author{
Troy Robertson
}
\note{}
\seealso{
	\code{\linkS4class{Period}, \link{getPeriod}, \link{getPeriodInfo}, \link{getPeriodActionMat}}
}
\examples{
## Return period timestep data matrix
# getPeriodElementTSData(period, 2, 1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
