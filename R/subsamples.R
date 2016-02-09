
#' Take subsamples from data.frame
#'
#' @param d object of class \code{\link{sim_data}}
#' @param n.sample subsample sizes
#' @param N number of subsamples of each size
#' @return list of length \code{N} with subsampled data frames
#' for each value of \code{n.sample}
#' @export
subsamples <- function(d, n.sample = seq(50, 500, 50), N = 10) {
  stopifnot(inherits(d, "sim_data"))
  ss  <- function(n) dplyr::sample_n(d, n)
  ss <- lapply(seq_len(N), function(null) lapply(n.sample, ss))
  structure(ss, n.sample = n.sample, N = N,
            real_Rsquared = attr(d, "Rsquared"), real_RMSE = attr(d, "RMSE"),
            class = c("subsamples", "list"))
}