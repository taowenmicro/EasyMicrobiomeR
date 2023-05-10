#' Confidence Interval
#' 
#' Calculates the confidence intervals, via the Mahalnobis Distance and use of the \code{\link[=ilr]{Log-Ratio}} Transformation
#' @inheritParams ggplot2::geom_smooth
#' @inheritParams ggplot2::geom_density2d
#' @inheritParams ggplot2::geom_point
#' @inheritParams ggplot2::geom_path
#' @param breaks the confidence intervals, default to 50, 90 and 95 percent.
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggtern:::rd_aesthetics("geom", "ConfidenceTern")}
#' @examples
#'   data(Feldspar)
#'   ggtern(data=Feldspar,aes(An,Ab,Or)) + 
#'     geom_point() + 
#'     geom_confidence_tern()
#' @aliases geom_confidence
#' @name   geom_confidence_tern
#' @rdname geom_confidence_tern
#' @author Nicholas Hamilton
#' @export
geom_confidence_tern <- function(mapping = NULL, data = NULL, stat = "ConfidenceTern",position = "identity", 
                                  ...,
                                  lineend = "butt", linejoin = "round", linemitre = 1,
                                  na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomConfidenceTern,
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
geom_confidence <- function(...){
  tern_dep('1.0.6.1',"Depreciated due to naming package naming standards, replaced by geom_confidence_tern")
  geom_confidence_tern(...)
}


#' @rdname geom_confidence_tern
#' @format NULL
#' @usage NULL
#' @export
GeomConfidenceTern <- ggproto(
  "GeomConfidenceTern", 
  GeomPath,
  default_aes = aes(colour = "#3366FF", size = 0.5, linetype = 1, alpha = NA))

