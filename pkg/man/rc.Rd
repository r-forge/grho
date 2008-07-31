\name{rc}
\alias{rc}
\title{Rate constant from exp. val. pars}
\description{
  Evaluate the corresponding rate constant as a function of the expected
  value parameters at three specific times - 24, 36 and 48 hours.
}
\usage{
rc(coefs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{coefs}{length 3 numeric vector of coefficients named
    \dQuote{\code{alpha}}, \dQuote{\code{beta}} and
    \dQuote{\code{gamma}}.
  }
}
%\details{}
\value{
  the rate constant as a numeric scalar
}
%\references{}
%\author{}
%\note{}
%\seealso{}
\examples{
rc(c(alpha = 0.28, beta = 0.35, gamma = 0.38))
}
\keyword{manip}
\keyword{regression}

