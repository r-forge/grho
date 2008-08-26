`Grho` <-
function(Time, alpha, beta, gamma)
{
    rho <- (gamma - alpha)/(beta - alpha) - 1
    stopifnot(0 < rho)
    rc <- -log(rho)/12
    Br2 <- (beta - alpha)/(rho - 1)
    A <- alpha - Br2
    B <- Br2/(rho*rho)
    unname(A + B * exp(- rc * Time))
}

