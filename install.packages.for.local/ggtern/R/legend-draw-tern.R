#' Key drawing functions
#'
#' Each Geom has an associated function that draws the key when the geom needs
#' to be displayed in a legend. These are the options built into ggplot2.
#'
#' @return A grid grob.
#' @param data A single row data frame containing the scaled aesthetics to
#'   display in this key
#' @param params A list of additional parameters supplied to the geom.
#' @param size Width and height of key in mm.
#' @author Nicholas Hamilton
#' @name draw_key_tern
#' @rdname draw_key_tern
NULL

#' @export
#' @rdname draw_key_tern
draw_key_crosshair_tern <- function(data, params, size) {
  dx = (1 - .ratio())/2
  segmentsGrob(c(0.5,0.5,0.5),
               c(0.5,0.5,0.5),
               c(dx,dx,1.0-dx),
               c(0.0,1.0,0.5),
               gp = gpar(
                 col = alpha(data$colour, data$alpha),
                 lwd = data$size * .pt,
                 lty = data$linetype,
                 lineend = "butt"
               ),
               arrow = params$arrow
  )
}

#' @export
#' @rdname draw_key_tern
draw_key_Tmark <- function(data,params,size){
  dx = (1 - .ratio())/2
  segmentsGrob(0.5,0.5,1.0-dx,0.5,
               gp = gpar(
                 col = alpha(data$colour, data$alpha),
                 lwd = data$size * .pt,
                 lty = data$linetype,
                 lineend = "butt"
               ),
               arrow = params$arrow
  )
}

#' @export
#' @rdname draw_key_tern
draw_key_Lmark <- function(data,params,size){
  dx = (1 - .ratio())/2
  segmentsGrob(0.5,0.5,dx,1.0,
               gp = gpar(
                 col = alpha(data$colour, data$alpha),
                 lwd = data$size * .pt,
                 lty = data$linetype,
                 lineend = "butt"
               ),
               arrow = params$arrow
  )
}

#' @export
#' @rdname draw_key_tern
draw_key_Rmark <- function(data,params,size){
  dx = (1 - .ratio())/2
  segmentsGrob(0.5,0.5,dx,0.0,
               gp = gpar(
                 col = alpha(data$colour, data$alpha),
                 lwd = data$size * .pt,
                 lty = data$linetype,
                 lineend = "butt"
               ),
               arrow = params$arrow
  )
}

#' @export
#' @rdname draw_key_tern
draw_key_Tline <- function(data,params,size){
  r = .ratio()/2
  segmentsGrob(.5-r,.5,.5+r,.5,
               gp = gpar(
                 col = alpha(data$colour, data$alpha),
                 lwd = data$size * .pt,
                 lty = data$linetype,
                 lineend = "butt"
               ),
               arrow = params$arrow
  )
}

#' @export
#' @rdname draw_key_tern
draw_key_Lline <- function(data,params,size){
  r = .ratio()/2
  segmentsGrob(.5 - r*sin(30*pi/180),.5 + r*cos(30*pi/180),
               .5 + r*sin(30*pi/180),.5 - r*cos(30*pi/180),
               gp = gpar(
                 col = alpha(data$colour, data$alpha),
                 lwd = data$size * .pt,
                 lty = data$linetype,
                 lineend = "butt"
               ),
               arrow = params$arrow
  )
}

#' @export
#' @rdname draw_key_tern
draw_key_Rline <- function(data,params,size){
  r = .ratio()/2
  segmentsGrob(.5 + r*sin(30*pi/180),.5 + r*cos(30*pi/180),
               .5 - r*sin(30*pi/180),.5 - r*cos(30*pi/180),
               gp = gpar(
                 col = alpha(data$colour, data$alpha),
                 lwd = data$size * .pt,
                 lty = data$linetype,
                 lineend = "butt"
               ),
               arrow = params$arrow
  )
}

#' @export
#' @rdname draw_key_tern
draw_key_Tiso <- function(data,params,size){
  dy = 0.5*(1 - tan(30*pi/180))
  segmentsGrob(.5,min(.ratio()+dy,1),.5,0+dy,
               gp = gpar(
                 col = alpha(data$colour, data$alpha),
                 lwd = data$size * .pt,
                 lty = data$linetype,
                 lineend = "butt"
               ),
               arrow = params$arrow
  )
}

#' @export
#' @rdname draw_key_tern
draw_key_Liso <- function(data,params,size){
  dy = 0.5*(1 - tan(30*pi/180))
  segmentsGrob(0,0+dy,.ratio()*cos(pi*30/180),.ratio()*sin(pi*30/180)+dy,
               gp = gpar(
                 col = alpha(data$colour, data$alpha),
                 lwd = data$size * .pt,
                 lty = data$linetype,
                 lineend = "butt"
               ),
               arrow = params$arrow
  )
}

#' @export
#' @rdname draw_key_tern
draw_key_Riso <- function(data,params,size){
  dy = 0.5*(1 - tan(30*pi/180))
  segmentsGrob(1,0+dy,1-.ratio()*cos(pi*30/180),.ratio()*sin(pi*30/180)+dy,
               gp = gpar(
                 col = alpha(data$colour, data$alpha),
                 lwd = data$size * .pt,
                 lty = data$linetype,
                 lineend = "butt"
               ),
               arrow = params$arrow
  )
}

#' @export
#' @rdname draw_key_tern
draw_key_point_swap <- function(data, params, size) {
  pointsGrob(0.5, 0.5,
             pch = data$shape,
             gp = gpar(
               col = alpha(data$fill, data$alpha),
               fill = alpha(data$colour, data$alpha),
               fontsize = data$size * .pt + data$stroke * .stroke / 2,
               lwd = data$stroke * .stroke / 2
             )
  )
}

.ratio = function(){ 0.5*tan(60*pi/180) }
