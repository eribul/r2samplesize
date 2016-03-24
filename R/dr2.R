
#' The approximative $R^2$ distribution
#'
#' Density and distribution function for the $R^2$ distribution.
#'
#' The true density of $R^2$ as the multiple correlation coefficient
#' was given by Fisher 1928 but is too complex to use in practice. An approximation is
#' given by the reference and its density function is implemented here.
#' The corresponding distribution function  \code{pr2} is given by numerical integration
#' of \code{d2r} using \code{\link{integrate}}.
#'
#' @param r2 multiple correlation coefficient ($\R^2$)
#' @param n sample size
#' @param p number of independent variables
#' @param rho2 true population multiple correlation coefficient ($\rho^2$)
#'
#' @return Numeric of lengt one.
#'
#' @seealso
#' Shieh, G. (2007). Improved shrinkage estimation of squared multiple correlation
#' coefficient and squared cross-validity coefficient. Organizational Research Methods.
#'
#' @export
#' @name R2_dist
#'
#' @examples
#' r2 <- seq(0, 1, .01)
#' plot(r2, dr2(r2, 10, 5, .2), type = "l")
dr2 <- function(r2, n, p, rho2) {
  a  <- (1 - rho2) ^ ((n - 1) / 2)
  b  <- Re(hypergeo::hypergeo((n - 1) / 2, (n - 1) / 2, p / 2, rho2 * r2))
  cc <- r2 ^ (p / 2 - 1) * (1 - r2) ^ ((n - p - 1) / 2 - 1)
  d  <- beta(p / 2, (n - p - 1) / 2)
  a * b * (cc / d)
}

#' @rdname R2_dist
#' @export
pr2 <- function(q, r2, n, p ,rho2) integrate(dr2, 0, q, r2 = r2, n = n, p = p, rho2 = rho2)$value