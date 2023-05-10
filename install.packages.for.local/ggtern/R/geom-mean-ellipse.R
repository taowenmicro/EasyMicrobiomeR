#' Mean Ellipse
#' 
#' Produce ellipses from a mean and a variance of ternary compositional data, based off the function 
#' included in the \code{\link{compositions}} package.
#' 
#' @inheritParams ggplot2::geom_smooth
#' @inheritParams ggplot2::geom_density2d
#' @inheritParams ggplot2::geom_point
#' @inheritParams ggplot2::geom_path
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggtern:::rd_aesthetics("geom", "MeanEllipse")}
#' @examples
#'   data(Feldspar)
#'   ggtern(data=Feldspar,aes(An,Ab,Or)) + 
#'     geom_point() + 
#'     geom_mean_ellipse()
#' @name   geom_mean_ellipse
#' @rdname geom_mean_ellipse
#' @author Nicholas Hamilton & Ashton Drew
#' @export
geom_mean_ellipse <- function(mapping = NULL, data = NULL, stat = "MeanEllipse",position = "identity", 
                                 ...,
                                 lineend = "butt", linejoin = "round", linemitre = 1,
                                 na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomMeanEllipse,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(
      lineend   = lineend,
      linejoin  = linejoin,
      linemitre = linemitre,
      na.rm     = na.rm,
      ...
    )
  )
}


#' @rdname geom_confidence_tern
#' @format NULL
#' @usage NULL
#' @export
GeomMeanEllipse <- ggproto(
  "GeomMeanEllipse", 
  GeomPath,
  default_aes = aes(colour = "#3366FF", size = 0.5, linetype = 1, alpha = NA))

