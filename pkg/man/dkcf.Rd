\name{dkcf}
\alias{dkcf}
\docType{data}
\title{NDF assay results}
\description{
  Results of assays of the Neutral Detergent Fibre digestion according
  to several methods.  Two feeds, alfalfa and straw, were used and the
  digestion of the NDF was measured with three methods at five time
  points in five replicates.
}
\usage{data(dkcf)}
\format{
  A data frame with 450 observations on the following 5 variables.
  \describe{
    \item{\code{Rep}}{replicate - a factor with levels \code{one},
      \code{two}, \code{three},  \code{four} and \code{five}}
    \item{\code{Method}}{assay method - a factor with levels \code{A},
      \code{B} and \code{C}}
    \item{\code{Feed}}{a factor with levels \code{AS} (alfalfa) and \code{Straw}}
    \item{\code{Time}}{time (hr) since innoculation (numeric)}
    \item{\code{NDFD}}{percentage of NDF according to the assay (numeric)}
  }
}
%\details{}
%\source{}
%\references{}
\examples{
str(dkcf)
## Design is balanced
ftable(xtabs(~ Rep + Method + Feed + Time, dkcf))
}
\keyword{datasets}
