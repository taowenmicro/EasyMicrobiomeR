#' Points (Colour and Fill Swapped), as for a scatterplot
#'
#' The \code{geom_point_swap} geometry is used to create scatterplots, however, this version swaps the colour and the 
#' fill mappings. Useful if the fill mapping is already occupied (say with existing polygon geometry), this
#' geometry will allow points of shape 21-25 to use colour mapping for the center colour, and fill mapping for the
#' border.
#'
#'@examples 
#'data(Feldspar)
#'ggtern(Feldspar,aes(Ab,An,Or)) + 
#'stat_confidence_tern(geom='polygon',aes(fill=..level..),color='white') + 
#'geom_mask() + 
#'geom_point_swap(aes(colour=T.C,shape=Feldspar),fill='black',size=5) +
#'scale_shape_manual(values=c(21,24)) +
#'scale_color_gradient(low='green',high='red') +
#'labs(title="Feldspar",color="Temperature",fill='Confidence')
#'@inheritParams ggplot2::geom_point
#'@author Nicholas Hamilton
#'@rdname geom_point_swap
#'@export
geom_point_swap <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       ...,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPointSwap,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_point_swap
#' @format NULL
#' @usage NULL
#' @export
GeomPointSwap <- ggproto("GeomPointSwap", Geom,
  required_aes = c("x", "y"),
  non_missing_aes = c("size", "shape"),
  default_aes = aes(
    shape = 19, colour = "black", size = 1.5, fill = NA, alpha = NA, stroke = 0.5
  ),
  draw_panel = function(data, panel_scales, coord, na.rm = FALSE) {
   coords <- coord$transform(data, panel_scales)
   ggint$ggname("geom_point",
          pointsGrob( coords$x, 
                      coords$y,
                      pch    = coords$shape,
                      gp     = gpar(
                        col  = alpha(coords$fill,   coords$alpha),
                        fill = alpha(coords$colour, coords$alpha),
                        fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
                        lwd = coords$stroke * .stroke / 2
                      )
      )
    )
  },
  draw_key = draw_key_point_swap
)
