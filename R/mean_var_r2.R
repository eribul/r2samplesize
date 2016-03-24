#' Mean and variance of coefficient of determination
#'
#' The distribution of $R^2$ (as an estimate of $\rho^2$, the multiple correlation
#' coefficient in a linear regression model) has been shown to have a
#' quite compicated formula whereas at least its mean and variance can be estimated
#' using a hypergeometric function.
#'
#' @param rho2 value of $\rho^2$ in population
#' @param n sample size
#' @param p number of independent variables used for
#' estimate of multiple correlation
#'
#' @return Mean/variance of the approximated $R^2$ distribution as given in reference.
#' @export
#' @name mean_r2
#' @seealso Algorithms based on (i) and (ii) in:
#' Wishart, J., Kondo, T., & Elderton, E. M. (1931). The mean and second moment
#' coefficient of the multiple correlation coefficient, in samples from a normal
#' population. Biometrika, 353-376.
#'
#' or:
#' Shieh, G. (2007). Improved shrinkage estimation of squared multiple correlation
#' coefficient and squared cross-validity coefficient. Organizational Research Methods.
#'
#' @examples
mean_r2 <- function(rho2, n, p) {
  1 - ((n - p - 1) / (n - 1)) * (1 - rho2) * Re(hypergeo::hypergeo(1, 1, .5 * (n + 1), rho2))
}

#' @rdname mean_r2
#' @export
var_r2 <- function(rho2, n, p) {
  (
    (((n - p) * (n - p + 2)) / ((n - 1) * (n + 1))) *
      (1 - rho2) *
      Re(hypergeo::hypergeo(2, 2, .5 * (n + 3), rho2))
  ) - (1 - mean_r2(rho2, n, p)) ^ 2
}