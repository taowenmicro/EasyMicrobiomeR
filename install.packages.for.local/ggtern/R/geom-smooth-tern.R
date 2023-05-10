#' Add a Smoothed Conditional Mean.
#'
#' Aids the eye in seeing patterns in the presence of overplotting.
#' \code{geom_smooth_tern} and \code{stat_smooth_tern} are effectively aliases: they
#' both use the same arguments. Use \code{geom_smooth_tern} unless you want to
#' display the results with a non-standard geom.
#'
#' @inheritParams ggplot2::geom_smooth
#' @param expand expand the range of values by this much (vector of length 2) when fullrange is set to TRUE
#' @examples 
#' data(Feldspar)
#' ggtern(data=Feldspar,aes(Ab,An,Or,group=Feldspar)) +  
#'   geom_smooth_tern(method=lm,fullrange=TRUE,colour='red') + 
#'   geom_point() +
#'   labs(title="Example Smoothing")
#' @author Nicholas Hamilton
#' @rdname geom_smooth_tern
#' @export
geom_smooth_tern <- function( mapping = NULL, data = NULL,position = "identity",
                              ...,
                              method = "auto", formula = y ~ x, se = TRUE,na.rm = FALSE,
                              show.legend = NA, inherit.aes = TRUE,expand=c(0.5,0.5)) {
  layer(
    data        = data,
    mapping     = mapping,
    stat        = "smoothTern",
    geom        = GeomSmoothTern,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(
      na.rm   = na.rm,
      method  = method,
      formula = formula,
      se      = se,
      expand  = expand,
      ...
    )
  )
}

#' @rdname geom_smooth_tern
#' @format NULL
#' @usage NULL
#' @export
GeomSmoothTern <- ggproto("GeomSmoothTern", Geom,
  draw_group = function(self,data, panel_scales, coord){
    ribbon      = transform(data, colour = NA)
    path        = transform(data, alpha  = NA)
    has_ribbon  = !is.null(data$ymax) && !is.null(data$ymin)
    gList(
      if (has_ribbon) .drawRibbonModified(ribbon, panel_scales, coord),
      GeomLine$draw_panel(path, panel_scales, coord)
    )
  },
  draw_key = draw_key_smooth,
  default_aes = aes(colour = "#3366FF", fill = "grey60", size = 1,linetype = 1, weight = 1, alpha = 0.4)
)


#Taken from ggplot2, need to add extra z term in the summarize part
.drawRibbonModified = function(data, panel_scales, coord, na.rm = FALSE) {
  #Prevent no visible bindings error: http://ahref.io/aUNt9
  x = y = ymin = ymax = zmin = zmax = ids = NULL 

  #Continue
  if (na.rm) data <- data[stats::complete.cases(data[c("x", "ymin", "ymax")]), ]
  
  #Order from left to right in cartesian space
  #data <- tlr2xy(data,coord)
  #data <- data[order(data$group, data$x), ]
  #data <- tlr2xy(data,coord,inverse=TRUE)
  
  # Check that aesthetics are constant
  aes <- unique(data[c("colour", "fill", "size", "linetype", "alpha")])
  if (nrow(aes) > 1) stop("Aesthetics can not vary with a ribbon")
  aes <- as.list(aes)
  
  missing_pos <- !stats::complete.cases(data[c("x", "ymin", "ymax")])
  ids <- cumsum(missing_pos) + 1
  ids[missing_pos] <- NA
  
  positions <- plyr::summarise(data, 
    x = c(x, rev(x)), y = c(ymax, rev(ymin)),z = c(zmax,rev(zmin)), id = c(ids, rev(ids)))
  munched <- coord_munch(coord, positions, panel_scales)
  
  ggint$ggname("geom_ribbon", polygonGrob(
    munched$x, munched$y, id = munched$id,
    default.units = "native",
    gp = gpar(
      fill = alpha(aes$fill, aes$alpha),
      col = alpha(aes$colour, aes$alpha),
      lwd = aes$size * .pt,
      lty = aes$linetype)
  ))
}
