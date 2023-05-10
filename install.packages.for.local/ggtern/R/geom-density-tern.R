#' Density Estimate (ggtern version)
#' 
#' Perform a 2D kernel density estimatation using kde2d and display the results with contours. This can be 
#' useful for dealing with overplotting. Additional weight aesthetic (see aesthetic section below) permits better weighting if desired
#' 
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggtern:::rd_aesthetics("geom", "density_tern")}
#' @inheritParams ggplot2::geom_point
#' @inheritParams ggplot2::geom_path
#' @inheritParams ggplot2::geom_density2d
#' @examples
#' #Plot Density Estimate, on isometric log ratio transformation of original data
#' data('Feldspar')
#' ggtern(Feldspar,aes(Ab,An,Or)) + 
#'  geom_density_tern(aes(color=..level..),bins=5) +
#'  geom_point()
#' 
#' @author Nicholas Hamilton
#' @rdname geom_density_tern
#' @export
geom_density_tern <- function(mapping = NULL, data = NULL, stat='DensityTern', position='identity',
                              ...,
                              lineend = "butt", linejoin = "round", linemitre = 1,
                              na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomDensityTern,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(
      na.rm     = na.rm,
      lineend   = lineend,
      linejoin  = linejoin,
      linemitre = linemitre,
      ...
    )
  )
}

#' @rdname geom_density_tern
#' @format NULL
#' @usage NULL
#' @export
GeomDensityTern <- ggproto("GeomDensityTern", 
                           GeomPath,
                           default_aes = aes(colour = "#3366FF", size = 0.5, linetype = 1, alpha = NA,weight=1.0))





