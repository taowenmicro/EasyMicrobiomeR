#' Zoom on Plot Region
#' 
#' A series of convenience functions for the zooming in on the middle or apex regions to various degrees.
#' In these convenience functions, a single value of \code{x} is expected, which defines the values of the apex
#' limits other than the point of reference, for example, \code{theme_zoom_T} will fix the \code{T} limit 
#' at \code{1}, and will adjust the balancing limits according to the argument x. Equivalent are also possible for
#' the \code{L} and \code{R} apexes, via the \code{theme_zoom_L} and \code{theme_zoom_R} functions respectively. 
#' Finally, the \code{theme_zoom_center} function will adjust all three apex limits, serving, as the name suggests, 
#' to act as a centred zoom. The examples below are fairly self explanatory. 
#' @aliases theme_zoom
#' @param x numeric scalar
#' @param ... additional arguments to be passed through to \code{\link{limit_tern}}
#' @examples
#' #Default Plot
#' data(Feldspar)
#' base = ggtern(Feldspar,aes(Ab,An,Or)) +
#'        theme_bw(8) +
#'        geom_density_tern() + 
#'        geom_point() + 
#'        labs(title="Original")
#' 
#' #Zoom on Left Region
#' A = base + theme_zoom_L(0.5) + labs(title="theme_zoom_L")
#' 
#' #Zoom on Right Region
#' B = base + theme_zoom_R(0.5) + labs(title="theme_zoom_R")
#' 
#' #Zoom on Top Region
#' C = base + theme_zoom_T(0.5) + labs(title="theme_zoom_T")
#' 
#' #Zoom on Center Region
#' D = base + theme_zoom_center(0.5) + labs(title="theme_zoom_center")
#' 
#' #Put all together for comparisons sake
#' grid.arrange(arrangeGrob(base), 
#'              arrangeGrob(A,B,nrow=1), 
#'              arrangeGrob(C,D,nrow=1), 
#'              ncol=1, heights=c(2,1,1),
#'              top = "Comparison of Zooming Functions")
#' 
#' @author Nicholas Hamilton
#' @rdname theme_zoom_X
#' @name theme_zoom_X
NULL

#' @rdname theme_zoom_X
#' @export
theme_zoom_T = function(x = 1.0,...){
  stopifnot(x > 0)
  args = list(...); args$L = args$R = x; args$T = 1
  do.call(limit_tern,args=args)
}

#' @rdname theme_zoom_X
#' @export
theme_zoom_L = function(x = 1.0,...){
  stopifnot(x > 0)
  args = list(...); args$T = args$R = x; args$L = 1
  do.call(limit_tern,args=args)
}

#' @rdname theme_zoom_X
#' @export
theme_zoom_R = function(x = 1.0,...){
  stopifnot(x > 0)
  args = list(...); args$T = args$L = x; args$R = 1
  do.call(limit_tern,args=args)
}

#' @rdname theme_zoom_X
#' @export
theme_zoom_center = function(x=1.0,...){
  stopifnot(x > 1/3)
  args = list(...); args$T = args$L = args$R = x
  do.call(limit_tern,args=args)
}

#' @rdname theme_zoom_X
#' @usage NULL
#' @format NULL
#' @export
theme_zoom_M = theme_zoom_center

