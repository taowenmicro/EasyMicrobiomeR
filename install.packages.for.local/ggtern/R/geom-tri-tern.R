#' Tribin (ggtern version).
#' 
#' Divides the plane into regular triangles, counts the number of cases in
#' each triangles, and then (by default) maps the number of cases to the triangle
#' fill.
#' 
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "hex")}
#' @inheritParams ggplot2::geom_hex
#' @param geom,stat Override the default connection between `geom_hex_tern` and `stat_hex_tern`
#' @param fun the scalar function to use for the statistic
#' @rdname geom_tri_tern
#' @examples
#' set.seed(1)
#' n  = 1000
#' df = data.frame(x  = runif(n),
#'                 y  = runif(n),
#'                 z  = runif(n),
#'                 wt = runif(n))
#'#Equivalent of Hexbin
#'ggtern(df,aes(x,y,z)) + 
#'    geom_tri_tern(bins=10,aes(fill=..count..)) + 
#'    geom_point(size=0.25)
#'
#'#Custom Function, Mean
#'ggtern(df,aes(x,y,z)) + 
#'    geom_tri_tern(bins=5,aes(fill=..stat..,value=wt),fun=mean) + 
#'    geom_point(size=0.25)
#' @export
geom_tri_tern <- function(mapping = NULL, data = NULL,
                          stat = "tri_tern", position = "identity",
                          ...,
                          fun = sum,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomTriTern,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(
      na.rm = na.rm,
      fun   = fun,
      ...
    )
  )
}


#' @rdname geom_hex_tern
#' @format NULL
#' @usage NULL
#' @export
GeomTriTern <- ggproto("GeomTriTern", Geom,
                       draw_group = function(data,panel_params,coord){
                         if (!inherits(coord, "CoordTern"))
                           stop("geom_tri_tern() only works with Ternary coordinates", call. = FALSE)
                         
                         #Transform
                         data = coord$transform(data, panel_params)
                         
                         #Polygon Grob
                         ggint$ggname("geom_tri_tern", 
                                      polygonGrob(data$x, 
                                                  data$y, 
                                                  default.units = "native",
                                                  gp = gpar(
                                                    col  = data$colour[1], 
                                                    fill = alpha(data$fill[1], 
                                                                 data$alpha[1]),
                                                    lty  = data$linetype[1], 
                                                    lwd  = data$size[1]
                                                  )
                                      )
                         )
                       },
                       required_aes = c("x", "y", "z"),
                       default_aes = aes(colour = NA, fill = "grey50", size = 0.5, alpha = 1, linetype=1),
                       draw_key = draw_key_polygon
)
