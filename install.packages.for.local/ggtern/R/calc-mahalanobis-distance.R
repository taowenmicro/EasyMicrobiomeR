#' Mahalanobis Distance
#' 
#' Modified version of the code provided in the \code{\link[chemometrics]{drawMahal}} package
#' @param  x data
#' @param  x.mean mean value
#' @param  x.cov coveriance value
#' @param  whichlines the confidence values
#' @param  m the number of values to return for each line
#' @return list containing mdX and mdY values.
#' @rdname mahalanobis_distance
#' @author Nicholas Hamilton
#' @export
mahalanobis_distance <- function(x,x.mean,x.cov,whichlines=c(0.975,0.90,0.75),m=360){    
  mdX       = matrix(NA,nrow=m,ncol=length(whichlines))
  mdY       = matrix(NA,nrow=m,ncol=length(whichlines))
  cov.svd   = svd(x.cov, nv = 0)
  r         = cov.svd[["u"]] %*% diag(sqrt(cov.svd[["d"]]))
  alphamd   = sqrt(qchisq(whichlines,2))
  for (j in 1:length(whichlines)){
    e1md    = cos(c(0:(m-1))/m * 2 * pi) * alphamd[j]
    e2md    = sin(c(0:(m-1))/m * 2 * pi) * alphamd[j]
    emd     = cbind(e1md, e2md)
    ttmd    = t(r %*% t(emd)) + rep(1, m) %o% x.mean
    mdX[,j] = ttmd[,1]
    mdY[,j] = ttmd[,2]
  }
  list(mdX = mdX,
       mdY = mdY)
}