#' Show or Hide Axis Ticklabels
#' 
#' Convenience functions to enable or disable the axis ticklabels
#' 
#' \code{theme_showlabels} is a function that apends to the current theme a flag to switch ON the axis ticklabels, whilst 
#' \code{theme_hidelabels} or \code{theme_nolabels} (Alias) are functions that apends to the current theme a flag 
#' to switch OFF the axis ticklabels
#' @author Nicholas Hamilton
#' @rdname theme_showlabels
#' @name theme_showlabels
NULL

#' @rdname theme_showlabels
#' @export
theme_showlabels <- function(){.theme_showlabels(TRUE)}

#' @rdname theme_showlabels
#' @export
theme_hidelabels <- function(){.theme_showlabels(FALSE)}

#' @rdname theme_showlabels
#' @export
theme_nolabels   <- theme_hidelabels

#Internal Function
.theme_showlabels <- function(show){
  theme(tern.axis.text.show=show)
}