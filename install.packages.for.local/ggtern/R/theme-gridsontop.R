#' Render Grids on Top
#' 
#' Convenience function to render the major and minor grids on top (or bottom) of the other layers.
#' By default the grids are rendered in the background (bottom)
#' @author Nicholas Hamilton
#' @rdname theme_gridsontop
#' @export
theme_gridsontop = function(){
  theme(tern.panel.grid.ontop=TRUE)
}

#' @rdname theme_gridsontop
#' @export
theme_gridsonbottom = function(){
  theme(tern.panel.grid.ontop=FALSE)
}