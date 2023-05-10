#' Annotation: High-performance rectangular tiling (ggtern version)
#'
#' This is a special version of \code{\link{geom_raster}} optimised for static
#' annotations that are the same in every panel. These annotations will not
#' affect scales (i.e. the x and y axes will not grow to cover the range
#' of the raster, and the raster must already have its own colours).
#'
#' Most useful for adding bitmap images.
#'
#' @param raster raster object to display
#' @param xmin,xmax x location (in npc coordinates) giving horizontal
#'   location of raster
#' @param ymin,ymax y location (in npc coordinates) giving vertical
#'   location of raster
#' @param interpolate If \code{TRUE} interpolate linearly, if \code{FALSE}
#'   (the default) don't interpolate.
#' @examples 
#' data(Feldspar)
#' data(FeldsparRaster)
#' ggtern(Feldspar,aes(Ab,An,Or)) + 
#' theme_rgbw() + 
#' annotation_raster_tern(FeldsparRaster,xmin=0,xmax=1,ymin=0,ymax=1) +
#' geom_mask() + 
#' geom_point(size=5,aes(shape=Feldspar,fill=Feldspar),color='black') +
#' scale_shape_manual(values=c(21,24)) +
#' labs(title="Demonstration of Raster Annotation")
#' @author Nicholas Hamilton
#' @rdname annotation_raster_tern
#' @export
annotation_raster_tern <- function(raster, xmin=0, xmax=1, ymin=0, ymax=1, interpolate = FALSE) {
  raster <- grDevices::as.raster(raster)
  layer(
    data = NULL,
    mapping = NULL,
    stat = StatIdentity,
    position = PositionIdentity,
    geom = GeomRasterAnnTern,
    inherit.aes = TRUE,
    params = list(
      raster = raster,
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      interpolate = interpolate
    )
  )
  
}

#' @rdname annotation_raster_tern
#' @format NULL
#' @usage NULL
#' @export
GeomRasterAnnTern <- ggproto("GeomRasterAnnTern", Geom,
                         extra_params = "",
                         handle_na = function(data, params) {
                           data
                         },
                         
                         draw_panel = function(data, panel_scales, coord, raster, xmin, xmax,
                                               ymin,ymax,interpolate = FALSE) {
                           if (!inherits(coord, "CoordTern")) {
                             stop("annotation_raster_tern only works with Ternary coordinates",
                                  call. = FALSE)
                           }
                           corners <- data.frame(x = c(xmin,(xmin+xmax/2),xmax),
                                                 y = c(ymin,ymax,ymin)*.ratio())
                           data    <- coord$transform(tlr2xy(corners,coord,inverse=TRUE), 
                                                      panel_scales)
                           x_rng   <- range(data$x, na.rm = TRUE)
                           y_rng   <- range(data$y, na.rm = TRUE)
                           rasterGrob(raster, 
                                      x_rng[1],y_rng[1],
                                      diff(x_rng),diff(y_rng), 
                                      default.units = "native",
                                      just = c("left","bottom"), interpolate = interpolate)
                         }
)
