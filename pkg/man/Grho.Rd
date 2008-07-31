\name{Grho}
\alias{Grho}
\title{Convert expected value parameters}
\description{
  For NDF assay data it is more convenient to parameterize the
  asymptotic regression model in terms of the expected values at three
  time points.  This function provides the fitted values at the
  indicated times according to the values of the expected values at
  three specific times - 24, 36 and 48 hours.
}
\usage{
Grho(Time, alpha, beta, gamma)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Time}{numeric vector of times at which the fitted values are to
    be evaluated.}
  \item{alpha}{Fitted value at 24 hr.}
  \item{beta}{Fitted value at 36 hr.}
  \item{gamma}{Fitted value at 48 hr.}
}
%\details{}
\value{
  A numeric vector, of the same length as the \code{Time} argument, of
  fitted values.
}
%\references{ ~put references to the literature/web site here ~ }
%\author{ ~~who you are~~ }
%\note{}
%\seealso{}
\examples{
Grho(c(24, 30, 48), 28, 35, 38)
xvals <- 20:50
plot(xvals, Grho(xvals, 28, 35, 38), type = "l",
     xlab = "Time (hr.)", ylab = "NDF digestion (\% of NDF)")
}
\keyword{regression}
\keyword{manip}
