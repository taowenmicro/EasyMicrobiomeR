#' Show or Hide the Ternary Arrows
#' 
#' \code{theme_noarrows} is a function that appends to the current theme a flag to switch OFF the ternary arrows
#' @author Nicholas Hamilton
#' @rdname theme_showarrows
#' @export
theme_noarrows   <- function(){.theme_arrows(FALSE)}

#' \code{theme_hidearrows} is an alias for \code{theme_noarrows}
#' @rdname theme_showarrows
#' @export
theme_hidearrows <- theme_noarrows

#' \code{theme_showarrows} is a function that appends to the current theme a flag to switch ON the ternary arrows
#' @rdname theme_showarrows
#' @export
theme_showarrows <- function(){.theme_arrows(TRUE)}

#internal function
.theme_arrows <- function(show){
  theme(tern.axis.arrow.show = show)
}