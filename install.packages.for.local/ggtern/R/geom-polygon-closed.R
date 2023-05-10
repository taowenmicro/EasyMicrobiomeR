#' Closed Polygons
#' 
#' A little like geom_area, in the sense that polygons are either upper or lower closed based 
#' on the starting and finishing points index.
#' 
#' @inheritParams ggplot2::geom_polygon
#' @param closure one of 'none','upper' or 'lower'
#' @author Nicholas Hamilton
#' @rdname geom_polygon_closed
#' @export
geom_polygon_closed <- function(mapping = NULL, data = NULL,
                                stat = "identity", position = "identity",
                                ...,
                                na.rm = FALSE,
                                show.legend = NA,
                                inherit.aes = TRUE,
                                closure='none') {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPolygonClosed,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      closure = closure,
      ...
    )
  )
}

#' @rdname geom_polygon_closed
#' @format NULL
#' @usage NULL
#' @export
GeomPolygonClosed <- ggproto("GeomPolygonClosed", GeomPolygon,
    draw_panel = function(data, panel_scales, coord, closure = 'none') {
      n <- nrow(data)
      if (n == 1) return(zeroGrob())
      
      munched <- coord_munch(coord, data, panel_scales)
      # Sort by group to make sure that colors, fill, etc. come in same order
      munched <- munched[order(munched$group), ]
      
      # For gpar(), there is one entry per polygon (not one entry per point).
      # We'll pull the first value from each group, and assume all these values
      # are the same within each group.
      first_idx <- !duplicated(munched$group)
      first_rows <- munched[first_idx, ]
      
      if(closure %in% c('upper','lower')){
        munched = ddply(munched,'group',function(df){
          row      = df[nrow(df),,drop=F]
          ix       = c(1,nrow(df))
          if(closure == 'upper'){
            row$x = max(df$x)
            row$y = max(df$y)
            rbind(df,row)
          }else{
            row$x = min(df$x)
            row$y = min(df$y)
            rbind(row,df)
          }
        })
      }
      
       polygonGrob(munched$x, munched$y, default.units = "native",
                   id = munched$group,
                   gp = gpar(
                     col = first_rows$colour,
                     fill = alpha(first_rows$fill, first_rows$alpha),
                     lwd = first_rows$size * .pt,
                     lty = first_rows$linetype
                   )
       )
    },
    required_aes = c("x", "y"),
    extra_params = c("na.rm",'closure')
)

