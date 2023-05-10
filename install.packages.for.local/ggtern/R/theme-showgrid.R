#' Show or Hide Grid
#' 
#' A set of convenience functions to enable or disable the use of major or minor (or both) gridlines. 
#' 
#' These flags operate at the 'rendering' level, and, supercede the presence of theme elements, therefore, 
#' 
#' \code{theme_hidegrid(...)} or its aliases will PREVENT rendering of grid elements, 
#' irrespective of whether those grid elements are valid (renderable). From the counter perspective, 
#' 
#' \code{theme_showgrid(...)} or its aliases will ALLOW rendering of grid elements, subject to those grid 
#' elements being valid (renderable, ie say \code{\link{element_line}} as opposed to \code{\link{element_blank}}). 
#' 
#' \code{theme_hidegrid} or \code{theme_nogrid} (alias) is a function which \strong{disables} both MAJOR and MINOR gridlines.
#' 
#' \code{theme_showgrid_major} is a function which \strong{enables} MAJOR gridlines.
#' 
#' \code{theme_hidegrid_major} or \code{theme_nogrid_major} (alias) is a function which \strong{disables} MAJOR gridlines.
#' 
#' \code{theme_showgrid_major} is a function which \strong{enables} MINOR gridlines.
#' 
#' \code{theme_hidegrid_minor} or \code{theme_nogrid_minor} (alias) is a function which \strong{disables} MINOR gridlines.
#' 
#' \code{theme_showgrid} is a function which \strong{enables} both MAJOR and MINOR gridlines.
#' 
#' @aliases theme_nogrid_minor theme_tern_nogrid_minor theme_hidegrid_minor theme_showgrid_minor
#' theme_nogrid_major theme_tern_nogrid_major theme_nogrid theme_tern_nogrid
#' 
#' @examples
#'   #Load data
#'   data(Feldspar)
#'   plot <- ggtern(data=Feldspar,aes(Ab,An,Or)) + 
#'           geom_point()   + #Layer 
#'           theme_bw()       #For clarity
#'   plot
#'   plot = plot + theme_hidegrid(); plot
#'   plot + theme_showgrid()
#' @author Nicholas Hamilton
#' @rdname theme_showgrid
#' @name theme_showgrid
NULL

#' @rdname theme_showgrid
#' @export
theme_showgrid       <- function(){theme_showgrid_major() + theme_showgrid_minor()}


#' @rdname theme_showgrid
#' @export
theme_hidegrid       <- function(){theme_hidegrid_major() + theme_hidegrid_minor()}

#' @rdname theme_showgrid
#' @export
theme_nogrid <- theme_hidegrid

#' @rdname theme_showgrid
#' @export
theme_tern_nogrid <- function(){
  tern_dep("1.0.1.3","theme_tern_nogrid() has been superceded by theme_nogrid()")
  theme_nogrid()
}

#' @rdname theme_showgrid
#' @export
theme_showgrid_major <- function(){.theme.showgrid.major(TRUE)}

#' @rdname theme_showgrid
#' @export
theme_hidegrid_major <- function(){.theme.showgrid.major(FALSE)}

#' @rdname theme_showgrid
#' @export
theme_nogrid_major <- theme_hidegrid_major

#' @rdname theme_showgrid
#' @export
theme_tern_nogrid_major <- function(){
  tern_dep("1.0.1.3","theme_tern_nogrid_major() has been superceded by theme_nogrid_major()")
  theme_nogrid_major()
}

#' @rdname theme_showgrid
#' @export
theme_showgrid_minor <- function().theme.showgrid.minor(TRUE)

#' @rdname theme_showgrid
#' @export
theme_hidegrid_minor <- function(){.theme.showgrid.minor(FALSE)}

#' @export
theme_nogrid_minor <- theme_hidegrid_minor

#' @export
theme_tern_nogrid_minor <- function(){
  tern_dep("1.0.1.3","theme_tern_nogrid_minor() has been superceded by theme_nogrid_minor()")
  theme_nogrid_minor()
}

#Internals
.theme.showgrid.major <- function(show){theme(tern.panel.grid.major.show = show)}
.theme.showgrid.minor <- function(show){theme(tern.panel.grid.minor.show = show)}
