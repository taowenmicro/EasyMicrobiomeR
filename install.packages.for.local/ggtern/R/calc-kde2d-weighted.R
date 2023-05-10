#' Two-Dimentional Kernel Density Estimation (Weighted)
#' 
#' Based of an algorithm found online at \code{https://stat.ethz.ch/pipermail/r-help/2006-June/107405.html}.
#' @aliases kde kde2dweighted
#' @keywords internal
#' @inheritParams MASS::kde2d
#' @param w numeric value or vector of same length as x (and y), for weighting, if not provided, 
#' a unified weighting of '1' will which will result in the regular density calculation.
#' @seealso \code{\link{kde2d}}
#' @author Nicholas Hamilton
#' @export
kde2d.weighted <- function (x, y, h, n = 25,lims = c(range(x), range(y)),w) {
  nx <- length(x)
  if (length(y) != nx) 
    stop("data vectors must be the same length")
  if (length(w) != nx & length(w) != 1)
    stop("weight vectors must be 1 or length of data")
  gx <- seq(lims[1], lims[2], length = n) # gridpoints x
  gy <- seq(lims[3], lims[4], length = n) # gridpoints y
  h <- if (missing(h)) 
      c(bandwidth.nrd(x), bandwidth.nrd(y))
  else rep(h,length.out = 2L)
  if (any(h <= 0))
      stop("bandwidths must be strictly positive")
  if (missing(w)) 
    w <- numeric(nx)+1;
  h <- h/4
  ax <- outer(gx, x, "-")/h[1] # distance of each point to each grid point in x-direction
  ay <- outer(gy, y, "-")/h[2] # distance of each point to each grid point in y-direction
  z <- (matrix(rep(w,n), nrow=n, ncol=nx, byrow=TRUE)*matrix(dnorm(ax), n, nx)) %*% t(matrix(dnorm(ay), n, nx))/(sum(w) * h[1] * h[2]) # z is the density
  return(list(x = gx, y = gy, z = z))
}