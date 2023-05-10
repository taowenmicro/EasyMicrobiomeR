#' Parse Labels w Latex Markup
#' 
#' A series of convenience functions that either enable or disable the use of the \code{\link{latex2exp}} package 
#' for parsing the various text elements using the \code{\link{TeX}} method. In many cases, by turning the latex parsing
#' on, this prevents confusing use of expressions to obtain greeks, superscripts, subscripts etc... Note that when 
#' latex parsing is enabled, this can override specific formatting directives from the element tree, see the third 
#' and fourth example below.
#' @param value logical as to whether to enable latex parsing or not
#' @examples 
#' 
#' #Demonstrate  without latex parsing
#' ggtern() + 
#'  theme_latex(FALSE) + 
#'  labs(title = '\\textit{Plot Title}')
#' 
#' #Same as before, but turn on the latex parsing
#' last_plot() + 
#'  theme_latex(TRUE)
#' 
#' #Demonstrate latex overriding the bold face
#' ggtern() + 
#'  labs(title = '\\textit{Plot Title}') + 
#'  theme_latex(TRUE) + 
#'  theme('plot.title' = element_text(face='bold'))
#' 
#' #Turn off latex parsing, bold title revealed
#' last_plot() + 
#'  theme_latex(FALSE)
#'  
#' @seealso \code{\link[latex2exp]{TeX}}
#' @author Nicholas Hamilton
#' @rdname theme_latex
#' @export
theme_latex = function(value = TRUE){ theme(tern.plot.latex = value) }

#' @rdname theme_latex
#' @export
theme_showlatex = function(){ theme_latex(TRUE) } 

#' @rdname theme_latex
#' @export
theme_nolatex = function(){ theme_latex(FALSE) }

#' @rdname theme_latex
#' @export
theme_hidelatex = function(){ theme_latex(FALSE) }
