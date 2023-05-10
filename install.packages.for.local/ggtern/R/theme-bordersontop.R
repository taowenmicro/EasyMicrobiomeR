#' Render Borders on Top
#' 
#' Convenience functions to render the axis border lines on top (or bottom) of the other layers. 
#' By default the borders are rendered in the background (bottom)
#' @author Nicholas Hamilton
#' @rdname theme_bordersontop
#' @export
theme_bordersontop = function(){
  tern_dep("2.1.2","theme_bordersontop() has been merged with theme_gridsontop()")
  #theme(tern.axis.line.ontop=TRUE)
  theme_gridsontop()
}

#' @rdname theme_bordersontop
#' @export
theme_bordersonbottom = function(){
  tern_dep("2.1.2","theme_bordersonbottom() has been merged with theme_gridsonbottom()")
  #theme(tern.axis.line.ontop=FALSE)
  theme_gridsonbottom()
}