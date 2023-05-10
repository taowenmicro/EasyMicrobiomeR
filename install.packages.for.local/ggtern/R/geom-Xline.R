#' Fixed Value Lines
#' 
#' Plot fixed value lines, for the top, left and right axis, analagous to the \code{\link{geom_hline}}
#' and \code{\link{geom_vline}} geometries in \code{\link[=ggplot]{ggplot2}}
#' 
#' @aliases geom_Tline geom_Lline geom_Rline Tline tline Lline lline Rline rline
#' @inheritParams ggplot2::geom_hline
#' @param Tintercept,Lintercept,Rintercept the intercepts for the T, L and R axis respectively
#' @examples 
#' ggtern() + 
#' geom_Tline(Tintercept=.5,arrow=arrow(), colour='red') + 
#' geom_Lline(Lintercept=.2, colour='green') + 
#' geom_Rline(Rintercept=.1, colour='blue')
#' @author Nicholas Hamilton
#' @rdname geom_Xline
#' @name geom_Xline
NULL

#' @rdname geom_Xline
#' @export
geom_Tline <- function(mapping = NULL, data = NULL,
                       ...,
                       Tintercept,
                       na.rm = FALSE, show.legend = NA) {
  
  # Act like an annotation
  if (!missing(Tintercept)) {
    data <- data.frame(Tintercept = Tintercept)
    mapping <- aes(Tintercept = Tintercept)
    show.legend=FALSE
  }
  
  layer(
    data        = data,
    mapping     = mapping,
    stat        = StatIdentity,
    geom        = GeomTline,
    position    = PositionIdentity,
    show.legend = show.legend,
    inherit.aes = FALSE,
    params      = list(
      na.rm     = na.rm,
      ...
    )
  )
}

#' @rdname  geom_Xline
#' @format NULL
#' @usage NULL
#' @export
GeomTline <- ggproto("GeomTline",Geom,
                     setup_data = function(self, data,params){
                       .setupXLineData(self, data, params, 'T');
                     },
                     draw_group = function(self,data,panel_params,coord){
                       .drawTRLLinesX(self,data,panel_params,coord,'T')
                     },
                     default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA,arrow=NULL),
                     required_aes = c("Tintercept"),
                     draw_key = draw_key_Tline
)

#' @rdname geom_Xline
#' @export
Tline <- geom_Tline

#' @rdname geom_Xline
#' @export
tline <- Tline

#' @rdname geom_Xline
#' @export
geom_Lline <- function(mapping = NULL, data = NULL,
                       ...,
                       Lintercept,
                       na.rm = FALSE, show.legend = NA) {
  
  # Act like an annotation
  if (!missing(Lintercept)) {
    data <- data.frame(Lintercept = Lintercept)
    mapping <- aes(Lintercept = Lintercept)
    show.legend=FALSE
  }
  
  layer(
    data        = data,
    mapping     = mapping,
    stat        = StatIdentity,
    geom        = GeomLline,
    position    = PositionIdentity,
    show.legend = show.legend,
    inherit.aes = FALSE,
    params      = list(
      na.rm     = na.rm,
      ...
    )
  )
}

#' @rdname  geom_Xline
#' @format NULL
#' @usage NULL
#' @export
GeomLline <- ggproto("GeomLline",Geom,
                     setup_data = function(self, data,params){
                       .setupXLineData(self, data, params, 'L');
                     },
                     draw_group = function(self,data,panel_params,coord){
                       .drawTRLLinesX(self,data,panel_params,coord,'L')
                     },
                     default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA,arrow=NULL),
                     required_aes = c("Lintercept"),
                     draw_key = draw_key_Lline
)

#' @rdname geom_Xline
#' @export
Lline <- geom_Lline

#' @rdname geom_Xline
#' @export
lline <- Lline

#' @rdname geom_Xline
#' @export
geom_Rline <- function(mapping = NULL, data = NULL,
                       ...,
                       Rintercept,
                       na.rm = FALSE, show.legend = NA) {
  
  # Act like an annotation
  if (!missing(Rintercept)) {
    data <- data.frame(Rintercept = Rintercept)
    mapping <- aes(Rintercept = Rintercept)
    show.legend=FALSE
  }
  
  layer(
    data        = data,
    mapping     = mapping,
    stat        = StatIdentity,
    geom        = GeomRline,
    position    = PositionIdentity,
    show.legend = show.legend,
    inherit.aes = FALSE,
    params      = list(
      na.rm     = na.rm,
      ...
    )
  )
}

#' @rdname  geom_Xline
#' @format NULL
#' @usage NULL
#' @export
GeomRline <- ggproto("GeomRline",Geom,
                     setup_data = function(self, data,params){
                       .setupXLineData(self, data, params, 'R');
                     },
                     draw_group = function(self,data,panel_params,coord){
                       .drawTRLLinesX(self,data,panel_params,coord,'R')
                     },
                     default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA,arrow=NULL),
                     required_aes = c("Rintercept"),
                     draw_key = draw_key_Rline
)

#' @rdname geom_Xline
#' @export
Rline <- geom_Rline


#' @rdname geom_Xline
#' @export
rline <- Rline

.setupXLineData = function(self, data, params, feat){
  #Put into cartesian
  coord       = coord_tern()
  
  axisNames = names(coord$mapping)
  if(!feat %in% axisNames) stop(sprintf("Invalid 'feat' variable ('%s'), please use %s",
                                        feat,
                                        joinCharacterSeries(axisNames,'or')),call.=FALSE)
  data      = remove_missing(data,vars=paste(axisNames,'intercept',sep=""),na.rm=TRUE,name=class(self)[1],finite=TRUE)
  if(empty(data)) return(zeroGrob())

  mapping   = coord$mapping
  
  #Get the correct sequence of other axes, relative to the featured axis
  getOthers = function(mapping,feat){
    others  = rep(names(mapping),2)
    ix.feat = which(others == feat)
    mapping[ (others[-ix.feat])[ ix.feat[1]+c(0,1) ] ]
  }; others = getOthers(mapping,feat)
  
  featIntercept = sprintf("%sintercept",feat)
  for(x in c(0:1) ){
    s = if(x == 0) "" else "end"
    limits = coord$scales[[ names(others)[2-x] ]]$limits %||% c(0,1)
    data[,sprintf("%s%s",mapping[[feat]],s) ] = data[,featIntercept]
    data[,sprintf("%s%s",  others[[1+x]],s) ] = 1 - data[, mapping[[feat]] ] - min(limits)
    data[,sprintf("%s%s",  others[[2-x]],s) ] = min(limits)
  }
  data
  
}

#internal function
.drawTRLLinesX <- function(self,data,panel_params, coord, feat){
  
  if(!'CoordTern' %in% class(coord)) return(zeroGrob())
  grob = zeroGrob()
  
  data = coord$transform(data, panel_params)
  
  tryCatch({
    cw   = calc_element('tern.axis.clockwise',coord$theme) ##Clockwise
    grob = segmentsGrob(if(!cw) data$x else data$xend, 
                        if(!cw) data$y else data$yend, 
                        if( cw) data$x else data$xend, 
                        if( cw) data$y else data$yend,
                        default.units     = "npc",
                        gp = gpar(col     = alpha(data$colour, data$alpha),
                                  fill    = alpha(data$colour, data$alpha),
                                  lwd     = data$size*find_global_tern(".pt"),
                                  lty     = data$linetype,
                                  lineend = 'butt'
                        ),
                        arrow = data$arrow)
  },error=function(e){message(as.character(e))})
  grob
}
