#' Change Axis labels and legend titles
#' 
#' New label modification functions, equivalent to the original functions in ggplot2 (\code{\link{xlab}} and \code{\link{ylab}}) 
#' however for the new axes used in the \code{ggtern} package
#' 
#' \code{Tlab} and \code{\link{xlab}} are equivalent (when \code{T='x'} in the \code{\link{coord_tern}} definition), 
#' as is \code{Llab} and \code{\link{ylab}} (when \code{L='y'}) , and \code{Rlab} and \code{zlab} (when \code{R='z'}), for other
#' assignments when \code{coord_tern} is defined, the equivalence is not the case, however, if \code{T='XXX'}, 
#' then \code{Tlab} will be the same as \code{XXXlab} (where \code{XXX} can be substituted for \code{'x', 'y' or 'z'}, and likewise for
#' \code{Llab} and \code{Rlab}). 
#' 
#' \code{zlab} is new to \code{ggtern}, but is intended to be an analogous to \code{xlab} and \code{ylab} 
#' as per the definitions in \code{ggplot2}.
#' 
#' @section Arrow Label:
#' \code{Tarrowlab, Larrowlab} and \code{Rarrowlab} permits setting a different label to the apex labels.
#' 
#' @section Arrow Label Suffix:
#' \code{Wlab} changes the ternary arrow suffix (ie atomic percent, weight percent etc) when the ternary arrows are enabled 
#' (see \code{\link{theme_showarrows}} and \code{\link{weight_percent}})
#' 
#' @section Precedence:
#' \code{AAAlab} takes precedence over \code{BBBlab} (where \code{AAA} represents \code{T, L or R} and \code{BBB} 
#' represents \code{x, y or z})
#' 
#' @section Use of Expressions:
#' Expressions can be used in the labels, in the event that the user wishes to render formula, 
#' subscripts or superscripts, see the last example below.
#' 
#' @section Creation of Aliasses:
#' Aliasses exist for \code{Tlab}, \code{Llab}, \code{Rlab} and \code{Wlab}, which are \code{tlab}, \code{llab}, \code{rlab} and \code{wlab}. 
#' These aliasses produce an identical result, and are there for convenience (as opposed to having an error thrown) 
#' in the event that the user forgets to use an upper-case letter.
#' 
#' Arguments for these functions can be provided as a \code{\link{character}} or \code{\link{expression}}, 
#' although other values can be inputed (such as, for example, scalar \code{\link{numeric}} or \code{\link{logical}}).
#' ggtern also imports the \code{\link{latex2exp}} package, and these formats can be parsed too.
#' 
#' @param label the desired label
#' @param labelarrow the desired label, if different to label, for the markers along the procession arrows 
#' @aliases llab tlab rlab wlab tarrowlab larrowlab rarrowlab ggtern-labels
#' @author Nicholas Hamilton
#' @rdname ggtern_labels
#' @name ggtern_labels
#' @seealso ggplot2 \code{\link[ggplot2]{labs}}
#' @examples
#' data(Feldspar)
#' plot <- ggtern(data=Feldspar,aes(Ab,An,Or)) +  geom_point() + 
#'         xlab("ABC") + ylab("DEF") + zlab("GHI")
#' 
#' #Alternatives, and Arrow Label
#' plot + Tlab("TOP") + Llab("LHS") + Rlab("RHS") + 
#'   Tarrowlab("Top Arrow Label") + Larrowlab("Left Arrow Label") + Rarrowlab("Right Arrow Label") +
#'   theme_showarrows() + Wlab("WEIGHT")
#' 
#' #Demonstrate the use of the latex2exp integration, and seperate arrow labels.  
#' ggtern(data=Feldspar,aes(x=Ab,y=An,z=Or)) + 
#' labs( x       = "NaAlSi_3O_8",
#'       xarrow  = "Albite, NaAlSi_3O_8",
#'       y       = "(Na,K)AlSi_3O_8",
#'       yarrow  = "Anorthite (Na,K)AlSi_3O_8",
#'       z       = "KAlSi_3O_8",
#'       zarrow  = "Orthoclase KAlSi_3O_8") + 
#' theme_latex(TRUE) + 
#' geom_point() + 
#' theme_showarrows() + 
#' theme_clockwise() + 
#' weight_percent()
NULL

#' \code{Tlab} modifies the label of the TOP apex species
#' @rdname ggtern_labels
#' @export
Tlab <- function(label,labelarrow=label){labs(T=label,Tarrow=labelarrow)}
tlab <- Tlab

#' \code{Llab} modifies the label of the LHS apex species
#' @rdname ggtern_labels
#' @export
Llab <- function(label,labelarrow=label){labs(L=label,Larrow=labelarrow)}
llab <- Llab

#' \code{Rlab} modifies the label of the RHS apex species
#' @rdname ggtern_labels
#' @export
Rlab <- function(label,labelarrow=label){labs(R=label,Rarrow=labelarrow)}
rlab <- Rlab

#' \code{Wlab} modifies the label of the arrow suffix
#' @rdname ggtern_labels
#' @export
Wlab <- function(label){labs(W=label)}
wlab <- Wlab

#' @rdname ggtern_labels
#' @export
zlab <- function(label){labs(z=label)}

#' @rdname ggtern_labels
#' @export
Tarrowlab <- function(label) labs(Tarrow = label)
tarrowlab <- Tarrowlab

#' @rdname ggtern_labels
#' @export
Larrowlab <- function(label) labs(Larrow = label)
larrowlab <- Larrowlab

#' @rdname ggtern_labels
#' @export
Rarrowlab <- function(label) labs(Rarrow = label)
rarrowlab <- Rarrowlab


