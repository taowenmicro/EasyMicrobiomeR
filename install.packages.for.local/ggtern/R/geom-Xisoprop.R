#' Fixed Value Isoproportion Lines
#' 
#' Create fixed isoproportion lines for each of the ternary axes, \code{geom_Xisoprop(...), (X = T, L, R)} will draw an isoproportion
#' line projecting from the T, L and R apex respectively. 
#' 
#'@section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggtern:::rd_aesthetics("geom", "Tisoprop")}
#' @inheritParams ggplot2::geom_segment
#' @param value, the isoproportion ratio to draw
#' @examples 
#' data(Feldspar)
#' ggtern(data=Feldspar,aes(Ab,An,Or)) +
#' geom_Tisoprop(value=0.5) +
#' geom_Lisoprop(value=0.5) +
#' geom_Risoprop(value=0.5) +
#' geom_point()
#' @author Nicholas Hamilton
#' @name geom_Xisoprop
#' @rdname geom_Xisoprop
NULL

#' @rdname geom_Xisoprop
#' @export
geom_Tisoprop <- function(mapping = NULL, data = NULL, 
                          ... , 
                          value,
                          na.rm = FALSE, 
                          show.legend = NA) {
  
  # Act like an annotation
  if (!missing(value)) {
    data        = data.frame(value = value)
    mapping     = aes(value = value)
    show.legend = FALSE
  }
  
  layer(
    data        = data,
    mapping     = mapping,
    stat        = StatIdentity,
    geom        = GeomTisoprop,
    position    = PositionIdentity,
    show.legend = show.legend,
    inherit.aes = FALSE,
    params      = list(
      na.rm     = na.rm,
      ...
    )
  )
}

#' @rdname geom_Xisoprop
#' @format NULL
#' @usage NULL
#' @export
GeomTisoprop <- ggproto("GeomTisoprop",Geom,
                        setup_data = function(self, data,params){
                          .setupIsopropData(self, data, params, 'T');
                        },
                        draw_group = function(self,data, panel_params,coord, arrow = NULL, lineend = "butt", na.rm = FALSE){
                          .drawTRLIsopropX(self,data,panel_params,coord,'T',arrow=arrow,lineend,na.rm)
                        },
                        default_aes     = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA,arrow=NULL),
                        required_aes    = c("value"),
                        draw_key        = draw_key_Tiso
)

#' @rdname geom_Xisoprop
#' @export
geom_Lisoprop <- function(mapping = NULL, data = NULL, 
                          ... , 
                          value,
                          na.rm = FALSE, 
                          show.legend = NA) {
  
  # Act like an annotation
  if (!missing(value)) {
    data        = data.frame(value = value)
    mapping     = aes(value = value)
    show.legend = FALSE
  }
  
  layer(
    data        = data,
    mapping     = mapping,
    stat        = StatIdentity,
    geom        = GeomLisoprop,
    position    = PositionIdentity,
    show.legend = show.legend,
    inherit.aes = FALSE,
    params      = list(
      na.rm     = na.rm,
      ...
    )
  )
}

#' @rdname geom_Xisoprop
#' @format NULL
#' @usage NULL
#' @export
GeomLisoprop <- ggproto("GeomLisoprop",Geom,
                        setup_data = function(self, data, params){
                          .setupIsopropData(self, data, params, 'L');
                        },
                        draw_group = function(self,data, panel_params,coord, arrow = NULL, lineend = "butt", na.rm = FALSE){
                          .drawTRLIsopropX(self,data,panel_params,coord,'L',arrow=arrow,lineend,na.rm)
                        },
                        default_aes  = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA, arrow=NULL),
                        required_aes = c("value"),
                        draw_key     = draw_key_Liso
)

#' @rdname geom_Xisoprop
#' @export
geom_Risoprop <- function(mapping = NULL, data = NULL, 
                          ... , 
                          value,
                          na.rm = FALSE, 
                          show.legend = NA) {
  
  # Act like an annotation
  if (!missing(value)) {
    data        = data.frame(value = value)
    mapping     = aes(value = value)
    show.legend = FALSE
  }
  
  layer(
    data        = data,
    mapping     = mapping,
    stat        = StatIdentity,
    geom        = GeomRisoprop,
    position    = PositionIdentity,
    show.legend = show.legend,
    inherit.aes = FALSE,
    params      = list(
      na.rm     = na.rm,
      ...
    )
  )
}

#' @rdname geom_Xisoprop
#' @format NULL
#' @usage NULL
#' @export
GeomRisoprop <- ggproto("GeomRisoprop",Geom,
                    setup_data = function(self, data,params){
                        .setupIsopropData(self, data, params, 'R');
                    },
                    draw_group = function(self,data, panel_params,coord, arrow = NULL, lineend = "butt", na.rm = FALSE){
                      .drawTRLIsopropX(self,data,panel_params,coord,'R',arrow=arrow,lineend,na.rm)
                    },
                    default_aes     = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA, arrow=NULL),
                    required_aes    = c("value"),
                    draw_key        = draw_key_Riso
)


.setupIsopropData = function(self, data, params, feat){
  #Put into cartesian
  coord       = coord_tern()
  
  axisNames = names(coord$mapping)
  if(!feat %in% axisNames) stop(sprintf("Invalid 'feat' variable ('%s'), please use %s",
                                        feat,
                                        joinCharacterSeries(axisNames,'or')),call.=FALSE)
  data = remove_missing(data,vars=paste(axisNames,'intercept',sep=""),na.rm=TRUE,name=class(self)[1],finite=TRUE)
  if(empty(data)) return(zeroGrob())
  
  mapping   = coord$mapping
  
  #Get the correct sequence of other axes, relative to the featured axis
  getOthers = function(mapping,feat){
    others  = rep(names(mapping),2)
    ix.feat = which(others == feat)
    mapping[ (others[-ix.feat])[ ix.feat[1]+c(0,1) ] ]
  }; others = getOthers(mapping,feat)
  
  cw   = calc_element('tern.axis.clockwise',coord$theme) ##Clockwise
  
  for(x in c(0:1) ){
    s = if(x == 0) "" else "end"
    data[,sprintf("%s%s",mapping[[feat]],s) ]                    = 1-x
    data[,sprintf("%s%s",  others[[ if(!cw) 1+x else 2-x ]],s) ]  = x*data$value
    data[,sprintf("%s%s",  others[[ if(!cw) 2-x else 1+x ]],s) ]  = x*(1-data$value)
  }
  
  ##Now plot the data
  data
}

#internal function
.drawTRLIsopropX <- function(self,data,panel_params, coord, feat, arrow = NULL, lineend = "butt", na.rm = FALSE){
  
  if(!'CoordTern' %in% class(coord)) return(zeroGrob())
  
  grob  = zeroGrob()
  if(empty(data)) return(grob)
  
  data = coord$transform(data, panel_params)
  
  tryCatch({
    grob = segmentsGrob(data$x,data$y,data$xend,data$yend, 
                        default.units     = "npc",
                        gp    = gpar(col     = alpha(data$colour,data$alpha),
                                     fill    = alpha(data$colour,data$alpha),
                                     lwd     = data$size*find_global_tern(".pt"),
                                     lty     = data$linetype,
                                     lineend = 'butt'),
                        arrow = arrow %||% data$arrow)
  },error=function(e){message(as.character(e))})
  grob
}
