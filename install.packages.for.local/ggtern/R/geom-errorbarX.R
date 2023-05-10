#' Ternary Error Bars
#' 
#' \code{geom_errorbarT}, \code{geom_errorbarL} and \code{geom_errorbarR} are geometries to render error bars
#' for the top, left and right apex species respectively, analogous to \code{\link[ggplot2]{geom_errorbar}} and/or 
#' \code{\link[ggplot2]{geom_errorbarh}} as provided in the base ggplot2 package.
#' 
#' @inheritParams ggplot2::geom_point
#' @inheritParams ggplot2::geom_segment
#' @section Aesthetics (geom_errorbarT): 
#' \Sexpr[results=rd,stage=build]{ggtern:::rd_aesthetics("geom", "errorbart")}
#' 
#' @section Aesthetics (geom_errorbarL):
#' \Sexpr[results=rd,stage=build]{ggtern:::rd_aesthetics("geom", "errorbarl")}
#' 
#' @section Aesthetics (geom_errorbarR):
#' \Sexpr[results=rd,stage=build]{ggtern:::rd_aesthetics("geom", "errorbarr")}
#' @examples
#' #Example with Dummy Data.
#' tmp <- data.frame(x=1/3,
#' y=1/3,
#' z=1/3,
#' Min=1/3-1/6,
#' Max=1/3+1/6)
#' ggtern(data=tmp,aes(x,y,z)) + 
#'   geom_point() + 
#'   geom_errorbarT(aes(Tmin=Min,Tmax=Max),colour='red')+
#'   geom_errorbarL(aes(Lmin=Min,Lmax=Max),colour='green')+
#'   geom_errorbarR(aes(Rmin=Min,Rmax=Max),colour='blue') 
#' @author Nicholas Hamilton
#' @rdname geom_errorbarX
#' @name geom_errorbarX
NULL

#' @rdname geom_errorbarX
#' @export
geom_errorbarT <- function(mapping = NULL, data = NULL, stat = "identity",position = "identity", 
                           ...,
                           arrow = NULL, lineend = "butt",
                           na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomErrorbart,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      arrow = arrow,  
      lineend  = lineend,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_errorbarX
#' @export
geom_errorbarL <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", arrow = NULL, lineend = "butt",
                           na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                           ...) {
  layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomErrorbarl,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      arrow = arrow,  
      lineend  = lineend,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_errorbarX
#' @export
geom_errorbarR <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", arrow = NULL, lineend = "butt",
                           na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                           ...) {
  layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomErrorbarr,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      arrow = arrow,  
      lineend  = lineend,
      na.rm = na.rm,
      ...
    )
  )
}



#' @rdname geom_errorbarX
#' @usage NULL
#' @format NULL
#' @export
GeomErrorbart <- ggproto("GeomErrorbart",
                         GeomSegment,
                         required_aes = c("x","y","z","Tmax","Tmin"),
                         draw_panel = function(self,data, panel_scales, coord,lineend='butt',arrow = NULL,na.rm=FALSE){
                           .render(self,data,panel_scales,coord,'T',lineend,arrow,na.rm)
                         }
)

#' @rdname geom_errorbarX
#' @usage NULL
#' @format NULL
#' @export
GeomErrorbarl <- ggproto("GeomErrorbarl",
                         GeomSegment,
                         required_aes = c("x","y","z","Lmax","Lmin"),
                         draw_panel = function(self, data, panel_scales, coord,lineend='butt',arrow = NULL,na.rm=FALSE){
                           .render(self,data,panel_scales,coord,'L',lineend,arrow,na.rm)
                         }
)
#' @rdname geom_errorbarX
#' @usage NULL
#' @format NULL
#' @export
GeomErrorbarr <- ggproto("GeomErrorbarr",
                         GeomSegment,
                         required_aes = c("x","y","z","Rmax","Rmin"),
                         draw_panel = function(self, data, panel_scales, coord,lineend='butt',arrow = NULL,na.rm=FALSE){
                           .render(self,data,panel_scales,coord,'R',lineend,arrow,na.rm)
                         }
)

#internal
.render = function(self, data, panel_scales, coord, 
                   feat=stop("feature variable required",call.=FALSE),
                   lineend='butt',arrow = NULL,na.rm=FALSE) {
  if(!"CoordTern" %in% class(coord)) return(zeroGrob())
  if(!"arrow"     %in% class(arrow)) arrow = arrow(angle=90,unit(0.01,'npc'))
  
  data <- remove_missing(data, na.rm = na.rm, c("x", "y","z", "linetype", "size", "shape"),name = "geom_errorbarX")
  if (empty(data)) return(zeroGrob())
  
  #Work out the mapping
  mapping   = coord$mapping
  total     = names(mapping)
  if(!feat %in% total | length(feat) > 1) stop("Invalid Feature Variable provided in the Data",call.=FALSE)
  
  #The Non-feature variables
  excl      = setdiff(total,feat)
  
  #Ensure the data is normalized
  #data = tlr2xy(tlr2xy(data,coord),coord,inverse = TRUE)
  
  #Determine the deltas
  deltaMin  = data[,mapping[[feat]]] - data[,sprintf("%smin",feat)]
  deltaMax  = data[,sprintf("%smax",feat)] - data[,mapping[[feat]]]
  
  for(x in excl){
    data[,sprintf('%smin',x)] = data[,mapping[[x]]] + deltaMin/2
    data[,sprintf('%smax',x)] = data[,mapping[[x]]] - deltaMax/2
  }
  
  for(nm in names(mapping)){
    colnames(data)[ which(colnames(data)==sprintf("%smin",nm)) ] = sprintf("%smin",mapping[[nm]])
    colnames(data)[ which(colnames(data)==sprintf("%smax",nm)) ] = sprintf("%smax",mapping[[nm]])
  }
  
  grobs = lapply(c('min','max'),function(x){
    df = data; colnames(df) = gsub(sprintf("(.*)(%s)",x),"\\1end",colnames(df))
    GeomSegment$draw_panel(df,panel_scales,coord, arrow=arrow, lineend=lineend, na.rm = na.rm)
  })
  
  #Build the Arrows
  gList(grobs[[1]],
        grobs[[2]])
}