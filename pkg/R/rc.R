`rc` <-
function(coefs)
{
    stopifnot(is.numeric(coefs), length(coefs) == 3,
              all(names(coefs) == c("alpha", "beta", "gamma")))
    rho <- (coefs[3] - coefs[1])/(coefs[2] - coefs[1]) - 1
    -log(rho)/12
}

