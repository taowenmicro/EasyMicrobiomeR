#' @include geom-text-viewport.R
NULL

#' Draw Label at Relative Position on Viewport
#' 
#' Since it is sometimes counter intuitive for working with ternary or other non-cartesian coordinates in the 
#' event that the the user wishes to place a label-geometry based on visual inspection, this geometry 
#' positions such text item at a fraction from x=[0,1] and y=[0,1] of the viewport in x and y cartesian 
#' coordinates.
#' 
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggtern:::rd_aesthetics("geom","Label")}
#' 
#' @examples
#' library(ggplot2)
#' data(Feldspar)
#' base = ggtern(data=Feldspar,aes(Ab,An,Or)) + 
#'   geom_mask() + 
#'   geom_point() + 
#'   geom_label_viewport(x=0.5,y=0.5,label="Middle",color='red') + 
#'   geom_label_viewport(x=1.0,y=1.0,label="Top Right",color='blue') + 
#'   geom_label_viewport(x=0.0,y=0.0,label="Bottom Left",color='green') +
#'   geom_label_viewport(x=0.0,y=1.0,label="Top Left",color='orange') + 
#'   geom_label_viewport(x=1.0,y=0.0,label="Bottom Right",color='magenta')
#' base
#' 
#' base + 
#'   geom_label_viewport(x=0.9,y=0.5,label="Clipping Turned Off",color='purple',hjust=0,clip='on') 
#' 
#' base + 
#'   geom_label_viewport(x=0.9,y=0.5,label="Clipping Turned Off",color='purple',hjust=0,clip='off') 
#' 
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_text
#' @inheritParams ggplot2::geom_point
#' @inheritParams ggplot2::geom_label
#' @param hjust horizontal justification
#' @param vjust vertical justification
#' @seealso \code{\link[ggplot2]{geom_label}}
#' @author Nicholas Hamilton
#' @rdname geom_label_viewport
#' @name geom_label_viewport
#' @export
geom_label_viewport <- function( mapping       = NULL, 
                                 data          = NULL,
                                 stat          = "identity", 
                                 position      = "identity",
                                 ...,
                                 hjust         = 'inward',
                                 vjust         = 'inward',
                                 parse         = FALSE,
                                 label.padding = unit(0.25, "lines"),
                                 label.r       = unit(0.15, "lines"),
                                 label.size    = 0.25,
                                 na.rm         = FALSE,
                                 show.legend   = NA,
                                 inherit.aes   = TRUE) {
  layer(
    data            = data,
    mapping         = mapping,
    stat            = stat,
    geom            = GeomLabelViewport,
    position        = position,
    show.legend     = show.legend,
    inherit.aes     = inherit.aes,
    params = list(
      parse         = parse,
      label.padding = label.padding,
      label.r       = label.r,
      label.size    = label.size,
      na.rm         = na.rm,
      hjust         = hjust,
      vjust         = vjust,
      ...
    )
  )
}

#' @rdname geom_label_viewport
#' @format NULL
#' @usage NULL
#' @export
GeomLabelViewport <- ggproto("GeomLabelViewport", 
  GeomLabel,
  setup_data = function(self, data, params){
    data$hjust = data$hjust %||% params$hjust
    data$vjust = data$vjust %||% params$vjust
    data
  },
  default_aes = defaults(
    aes(x='x',y='y'),
    GeomLabel$default_aes
  ),
  draw_panel = function(self, data, panel_scales, coord, parse = FALSE,
                         na.rm = FALSE,
                         label.padding = unit(0.25, "lines"),
                         label.r = unit(0.15, "lines"),
                         label.size = 0.25,
                         clip = "inherit") {
    
    #Check the required aesthetics have been provided
    ggint$check_required_aesthetics(self$required_aes, names(data), ggint$snake_class(self))
  
    #Compute hjust and vjust, if provided in text format
    if (is.character(data$vjust))
      data$vjust <- ggint$compute_just(data$vjust, data$y)
    if (is.character(data$hjust))
      data$hjust <- ggint$compute_just(data$hjust, data$x)
    
    #Bind to the viewport Limits
    data$x = with(data,pmin(pmax(x,0.0),1.0))
    data$y = with(data,pmin(pmax(y,0.0),1.0))
    
    #Remove Duplicated Rows
    uniqueOn = unique(c('PANEL','group',names(self$default_aes),self$required_aes))
    data     = data[!duplicated(data[,intersect(names(data),uniqueOn)]), ]
    
    grobs <- lapply(1:nrow(data), function(i) {
       row <- data[i, , drop = FALSE]
       lab <- row$lab
       if (parse){
         lab <- parse(text = as.character(lab))
       }
       
       ggint$labelGrob(
         label   = lab,
         x       = unit(row$x, "native"),
         y       = unit(row$y, "native"),
         just    = c(row$hjust, row$vjust),
         padding = label.padding,
         r       = label.r,
         text.gp = gpar(
           col        = row$colour,
           fontsize   = row$size * .pt,
           fontfamily = row$family,
           fontface   = row$fontface,
           lineheight = row$lineheight
         ),
         rect.gp = gpar(
           col        = row$colour,
           fill       = alpha(row$fill, row$alpha),
           lwd        = label.size * .pt
         ),
         vp = grid::viewport(clip = clip)
       )
     })
     class(grobs) <- "gList"
     ggint$ggname("geom_label", grobTree(children = grobs))
   }
)


