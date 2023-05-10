#' Place Ticks Inside or Outside
#' 
#' \code{theme_ticksoutside} is a function that ensures the ticks are placed OUTSIDE of the plot area, whereas, 
#' \code{theme_ticksinside} is a function that ensures the ticks are placed INSIDE of the plot area 
#' (opposite to \code{theme_ticksoutside})
#' @author Nicholas Hamilton
#' @rdname theme_ticksoutside
#' @export
theme_ticksoutside <- function(){.theme_ticksoutside(TRUE)}

#' @rdname theme_ticksoutside
#' @export
theme_ticksinside  <- function(){.theme_ticksoutside(FALSE)}

#Internal
.theme_ticksoutside <- function(x){
  theme(tern.axis.ticks.outside=x)
}