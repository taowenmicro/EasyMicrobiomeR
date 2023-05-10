#' Show or Hide the Clipping Mask
#' 
#' Convenience Function to Show or Hide the Clipping Mask, \code{theme_showmask} is a function that 
#' appends to the current theme a flag to switch ON the clipping mask, whilst, \code{theme_nomask} (or \code{theme_hidemask}) is a function 
#' that appends to the current theme a flag to switch OFF the clipping mask
#' @author Nicholas Hamilton
#' @rdname theme_showmask
#' @export
theme_nomask   <- function(){.theme_mask(FALSE)}

#' @rdname theme_showmask
#' @export
theme_hidemask <- theme_nomask

#' @rdname theme_showmask
#' @export
theme_showmask <- function(){.theme_mask(TRUE)}

#internal function
.theme_mask = function(show){
  theme(tern.panel.mask.show = show)
}