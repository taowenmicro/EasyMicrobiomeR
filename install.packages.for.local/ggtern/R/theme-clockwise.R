#' Direction of Ternary Rotation
#' 
#' \code{theme_clockwise, theme_anticlockwise} (or their aliasses) are function that instructs the axes 
#' precession to be clockwise or anticlockwise respectively.
#' 
#' If the \code{tern.axis.arrow.show} value is \code{FALSE}, these functions will set it to \code{TRUE}.
#' @author Nicholas Hamilton
#' @rdname theme_clockwise
#' @name theme_clockwise
NULL

#' @rdname theme_clockwise
#' @export
theme_clockwise        <- function(){.tern_clockwise(TRUE)}

#' @rdname theme_clockwise
#' @export
theme_anticlockwise    <- function(){.tern_clockwise(FALSE)}

#' @rdname theme_clockwise
#' @export
theme_counterclockwise <- function(){.tern_clockwise(FALSE)}

#' @rdname theme_clockwise
#' @usage NULL
#' @format NULL
#' @export
tern_clockwise         <- function(){.tern_clockwise(TRUE)}

#' @rdname theme_clockwise
#' @usage NULL
#' @format NULL
#' @export
tern_anticlockwise     <- function(){.tern_clockwise(FALSE)}

#' @rdname theme_clockwise
#' @usage NULL
#' @format NULL
#' @export
tern_counterclockwise  <- function(){.tern_clockwise(FALSE)}


.tern_clockwise <- function(clockwise){theme(tern.axis.clockwise=clockwise)}
