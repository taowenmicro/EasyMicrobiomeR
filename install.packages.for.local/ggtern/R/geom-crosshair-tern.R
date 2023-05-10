#' Ternary Crosshairs
#' 
#' A new geometry, \code{geom_crosshair_tern} is one that that marks on the respective axes, 
#' the values of each data point. We also include additional geometries \code{geom_Tmark}, 
#' \code{geom_Rmark} and \code{geom_Lmark} -- to render only the respective axis component 
#' of the abovementioned crosshair.
#' 
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggtern:::rd_aesthetics("geom", "crosshair_tern")}
#' @inheritParams ggplot2::geom_segment
#' @examples 
#' set.seed(1)
#' df = data.frame(x=runif(10),y=runif(10),z=runif(10))
#' base = ggtern(df,aes(x,y,z)) + geom_point()
#' base + geom_crosshair_tern()
#' base + geom_Tmark()
#' base + geom_Rmark()
#' base + geom_Lmark()
#' @author Nicholas Hamilton
#' @name geom_crosshair_tern
#' @rdname geom_crosshair_tern
NULL


#' @rdname geom_crosshair_tern
#' @export
geom_crosshair_tern <- function(mapping = NULL, data = NULL, stat = "identity",position = "identity", 
                                ...,
                                arrow = NULL, lineend = "butt",
                                na.rm = FALSE, show.legend = NA, inherit.aes = TRUE){
  layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomCrosshairTern,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(
      na.rm     = na.rm,
      arrow     = arrow,
      ...
    )
  )
}

#' @rdname geom_crosshair_tern
#' @format NULL
#' @usage NULL
#' @export
GeomCrosshairTern <- ggproto("GeomCrosshairTern",Geom,
                     setup_data = function(self,data, params) {
                       rbind(
                         .setupCrosshairData(self, data, params, 'T'),
                         .setupCrosshairData(self, data, params, 'L'),
                         .setupCrosshairData(self, data, params, 'R')
                       )
                     },
                     draw_group = function(self, data, panel_params, coord, arrow = NULL, lineend = "butt", na.rm = FALSE ){
                       ret = gList()
                       ret[[1]] = .drawTernaryCrosshair(self,data,panel_params,coord,'T',arrow,lineend,na.rm)
                       ret[[2]] = .drawTernaryCrosshair(self,data,panel_params,coord,'L',arrow,lineend,na.rm)
                       ret[[3]] = .drawTernaryCrosshair(self,data,panel_params,coord,'R',arrow,lineend,na.rm)
                       ret
                     },
                     default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA),
                     required_aes = c("x","y","z"),
                     draw_key = draw_key_crosshair_tern 
)


#' @rdname geom_crosshair_tern
#' @export
geom_Tmark <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", arrow = NULL, lineend = "butt",
                                na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                                ...){
  layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomTmark,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(
      na.rm     = na.rm,
      arrow     = arrow,
      ...
    )
  )
}

#' @rdname geom_crosshair_tern
#' @format NULL
#' @usage NULL
#' @export
GeomTmark <- ggproto("GeomTmark",Geom,
                     setup_data = function(self,data, params) {
                       .setupCrosshairData(self, data, params, 'T');
                     },
                     draw_group = function(self, data, panel_params, coord, arrow = NULL, lineend = "butt", na.rm = FALSE ){
                       .drawTernaryCrosshair(self,data,panel_params,coord,'T',arrow,lineend,na.rm)
                     },
                     default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA),
                     required_aes = c("x","y","z"),
                     draw_key = draw_key_Tmark
)


#' @rdname geom_crosshair_tern
#' @export
geom_Lmark <- function(mapping = NULL, data = NULL, stat = "identity",
                       position = "identity", arrow = NULL, lineend = "butt",
                       na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                       ...){
  layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomLmark,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(
      na.rm     = na.rm,
      arrow     = arrow,
      ...
    )
  )
}

#' @rdname geom_crosshair_tern
#' @format NULL
#' @usage NULL
#' @export
GeomLmark <- ggproto("GeomLmark",Geom,
                      setup_data = function(self,data, params) {
                        .setupCrosshairData(self, data, params, 'L');
                      },
                      draw_group = function(self, data, panel_params, coord, arrow = NULL, lineend = "butt", na.rm = FALSE ){
                        .drawTernaryCrosshair(self,data,panel_params,coord,'L',arrow,lineend,na.rm)
                      },
                      default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA),
                      required_aes = c("x","y","z"),
                      draw_key = draw_key_Lmark
  )

#' @rdname geom_crosshair_tern
#' @export
geom_Rmark <- function(mapping = NULL, data = NULL, stat = "identity",
                       position = "identity", arrow = NULL, lineend = "butt",
                       na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                       ...){
  layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomRmark,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(
      na.rm     = na.rm,
      arrow     = arrow,
      ...
    )
  )
}

#' @rdname geom_crosshair_tern
#' @format NULL
#' @usage NULL
#' @export
GeomRmark <- ggproto("GeomRmark",Geom,
                     setup_data = function(self,data, params) {
                       .setupCrosshairData(self, data, params, 'R');
                     },
                     draw_group = function(self, data, panel_params, coord, arrow = NULL, lineend = "butt", na.rm = FALSE ){
                       .drawTernaryCrosshair(self,data,panel_params,coord,'R',arrow,lineend,na.rm)
                     },
                     default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA),
                     required_aes = c("x","y","z"),
                     draw_key = draw_key_Rmark
)

.setupCrosshairData <- function(self, data, params, feat){
  
  data[,self$required_aes] = as.data.frame(acomp(data[,self$required_aes]))
  
  #Put into cartesian
  coord       = coord_tern()
  
  #Check Asis Names
  axisNames = names(coord$mapping)
  if(missing(feat) || !feat %in% axisNames) 
    stop(sprintf("Invalid 'feat' variable ('%s'), please use %s",feat,joinCharacterSeries(axisNames,'or')),call.=FALSE)
  
  #Remove Missing Data
  data      = remove_missing(data,vars=paste(axisNames,'intercept',sep=""),na.rm=TRUE,name=class(self)[1],finite=TRUE)
  if(empty(data)) return(zeroGrob())
  
  mapping   = coord$mapping
  
  #Get the correct sequence of other axes, relative to the featured axis
  getOthers = function(mapping,feat){
    others  = rep(names(mapping),2)
    ix.feat = which(others == feat)
    mapping[ (others[-ix.feat])[ ix.feat[1]+c(0,1) ] ]
  }; others = getOthers(mapping,feat)
  
  cw = calc_element('tern.axis.clockwise',coord$theme) ##Clockwise
  limits = coord$scales[[ names(others)[1] ]]$limits %||% c(0,1)
  data[,sprintf("%send",mapping[[feat]]) ]                = data[, mapping[[feat]] ]
  data[,sprintf("%send",others[[ 2 - cw ]]) ] = 1 - data[, mapping[[feat]] ] - min(limits)
  data[,sprintf("%send",others[[ 1 + cw ]]) ] = min(limits)
  
  data
}

#internal function
.drawTernaryCrosshair <- function(self,data,panel_params, coord, feat, arrow = NULL, lineend = "butt", na.rm = FALSE){
  if(!'CoordTern' %in% class(coord)) return(zeroGrob())
  
  data = coord$transform(data, panel_params)
  
  grob = zeroGrob()
  tryCatch({
    grob = segmentsGrob(data$x,data$y,data$xend,data$yend,
                        default.units     = "npc",
                        gp = gpar(col     = alpha(data$colour, data$alpha),
                                  fill    = alpha(data$colour, data$alpha),
                                  lwd     = data$size*find_global_tern(".pt"),
                                  lty     = data$linetype,
                                  lineend = lineend
                        ),
                        arrow = arrow)
  },error=function(e){message(as.character(e))})
  grob
}
