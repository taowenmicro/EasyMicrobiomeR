#' Modify the Ticklengths
#' 
#' Convenience Function for changing the major and/or minor ticklengths.
#' @param major,minor lenth of major and minor ticklengths respectively. 
#' Must be a unit object, or will be ignored.
#' @examples 
#' ggtern() +
#'   theme_ticklength(major = unit(5.0,'mm'),
#'                    minor = unit(2.5,'mm'))
#' @author Nicholas Hamilton
#' @rdname theme_ticklength
#' @name theme_ticklength
#' @export
theme_ticklength = function(major=NULL,minor=NULL){
  args = list()
  if(is.unit(major))
    args$tern.axis.ticks.length.major = major
  if(is.unit(minor))
    args$tern.axis.ticks.length.minor = minor
  do.call(theme,args)
}

#'@rdname theme_ticklength
#'@export
theme_ticklength_major = function(major){
  theme_ticklength(major,NULL)
}

#'@rdname theme_ticklength
#'@export
theme_ticklength_minor = function(minor){
  theme_ticklength(NULL,minor)
}

#' Show or Hide the Primary/Secondary Ticks
#' 
#' Convenience functions to enable or disable the axis primary or secondary ticks.
#' 
#' In \code{ggtern}, the primary ticks are deemed as being the ticks along the binary axis increasing to the apex species, primary ticks can consist of 
#' both major and minor ticks (major ticks have labels, and are generally longer and bolder). Therefore, there are three (3) sets of major primary ticks, and,
#' three (3) sets of minor primary ticks. 
#' 
#' These convenience functions introduce the concept of secondary ticks, which, are the same items however on the 
#' 'opposing' binary axis. 
#' 
#' For example, considering the TOP apex species, in a plot with 'clockwise' axis precession, the primary ticks would run along the 
#' LHS, whilst, the secondary ticks, woudl run along the RHS. By default, the primary ticks are switched ON, whilst the secondary ticks are switched OFF and are
#' controlled by the \code{tern.axis.ticks.primary.show} and \code{tern.axis.ticks.secondary.show} theme elements respectively.
#' 
#' \code{theme_showsecondary} is a function that apends to the current theme a flag to switch ON the secondary ticks
#' \code{theme_showticks(), themehideticks(), theme_noticks()} are functions that switch ON or OFF BOTH the primary or secondary ticks.
#' \code{theme_nosecondary} or \code{theme_hidesecondary} (Alias) are functions that apends to the current theme a flag to switch OFF the secondary ticks
#' \code{theme_showprimary} is a function that apends to the current theme a flag to switch ON the primary ticks
#' \code{theme_noprimary} or \code{theme_hideprimary} (Alias) are functions that apends to the current theme a flag to switch OFF the primary ticks
#' 
#' @examples
#' data(Feldspar)
#' plot <- ggtern(data=Feldspar,aes(Ab,An,Or)) + geom_point() + 
#'   theme_showsecondary()
#' @author Nicholas Hamilton
#' @rdname theme_showprimary
#' @name theme_showprimary
NULL

#' @rdname theme_showprimary
#' @export
theme_noprimary   <- function(){.theme_showprimary(FALSE)}

#' @rdname theme_showprimary
#' @export
theme_hideprimary <- theme_noprimary

#' @rdname theme_showprimary
#' @export
theme_showprimary <- function(){.theme_showprimary(TRUE)}

#' @rdname theme_showprimary
#' @export
theme_nosecondary   <- function(){.theme_showsecondary(FALSE)}

#' @rdname theme_showprimary
#' @export
theme_hidesecondary <- theme_nosecondary

#' @rdname theme_showprimary
#' @export
theme_showsecondary <- function(){.theme_showsecondary(TRUE)}

#' @rdname theme_showprimary
#' @export
theme_showticks <- function(){theme_showprimary() + theme_showsecondary()}

#' @rdname theme_showprimary
#' @export
theme_hideticks <- function(){theme_hideprimary() + theme_hidesecondary()}

#' @rdname theme_showprimary
#' @export
theme_noticks   <- theme_hideticks

#internals
.theme_showsecondary <- function(show){theme(tern.axis.ticks.secondary.show = show)}
.theme_showprimary   <- function(show){theme(tern.axis.ticks.primary.show   = show)}
