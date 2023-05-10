#' Create an annotation layer (ggtern version).
#'
#' This function adds geoms to a plot. Unlike typical a geom function,
#' the properties of the geoms are not mapped from variables of a data frame,
#' but are instead passed in as vectors. This is useful for adding small annotations
#' (such as text labels) or if you have your data in vectors, and for some
#' reason don't want to put them in a data frame.
#'
#' Note that all position aesthetics are scaled (i.e. they will expand the
#' limits of the plot so they are visible), but all other aesthetics are
#' set. This means that layers created with this function will never
#' affect the legend.
#'
#' @param geom name of geom to use for annotation
#' @param x,y,z,xmin,ymin,zmin,xmax,ymax,zmax,xend,yend,zend positioning aesthetics -
#'   you must specify at least one of these.
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @seealso \code{\link[ggplot2]{annotate}}
#' @examples 
#' ggtern() + 
#' annotate(geom  = 'text',
#'               x     = c(0.5,1/3,0.0),
#'               y     = c(0.5,1/3,0.0),
#'               z     = c(0.0,1/3,1.0),
#'               angle = c(0,30,60),
#'               vjust = c(1.5,0.5,-0.5),
#'               label = paste("Point",c("A","B","C")),
#'               color = c("green","red",'blue')) +
#'   theme_dark() + 
#'   theme_nomask()
#' @author Nicholas Hamilton
#' @rdname annotate
#' @name annotate
#' @export
annotate <- function( geom, 
                      x = NULL, y = NULL, z = NULL,
                      xmin = NULL, xmax = NULL,
                      ymin = NULL, ymax = NULL, 
                      zmin = NULL, zmax = NULL,
                      xend = NULL, yend = NULL, zend=NULL, ...,
                      na.rm = FALSE) {

  position <- compact(list(
    x = x, xmin = xmin, xmax = xmax, xend = xend,
    y = y, ymin = ymin, ymax = ymax, yend = yend,
    z = z, zmin = zmin, zmax = zmax, zend = zend
  ))
  aesthetics <- c(position, list(...))
  
  # Check that all aesthetic have compatible lengths
  lengths <- vapply(aesthetics, length, integer(1))
  unequal <- length(unique(setdiff(lengths, 1L))) > 1L
  if (unequal) {
    bad <- lengths != 1L
    details <- paste(names(aesthetics)[bad], " (", lengths[bad], ")",
                     sep = "", collapse = ", ")
    stop("Unequal parameter lengths: ", details, call. = FALSE)
  }
  
  data <- data.frame(position)
  layer(
    geom = geom,
    params = list(
      na.rm = na.rm,
      ...
    ),
    stat        = StatIdentity,
    position    = PositionIdentity,
    data        = data,
    mapping     = aes_all(names(data)),
    inherit.aes = FALSE,
    show.legend = FALSE,
    check.aes   = FALSE
  )
}

