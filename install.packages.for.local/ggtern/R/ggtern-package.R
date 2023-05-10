#' Ternary Diagrams in R
#'
#' @description
#' Ternary diagrams are used frequently in a number of disciplines to graph compositional features for mixtures of three different elements or compounds. 
#' It is possible to represent a coordinate system having three (3) degrees of freedom, in 2D space, since the third dimention is linear and depends only 
#' on the other two. 
#' 
#' The \code{ggtern} package is based on (extends) the very popular \code{\link[=ggplot]{ggplot2}} package, which is an implementation of Wilkinsons 
#' "The Grammar of Graphics", and, makes provision for a highly methodical construction process for the development 
#' of meaningful (graphical) data representations. Of course, the above book by Wilkinson outlines the \emph{theory}, 
#' whilst Hadley Wickhams \code{\link[=ggplot]{ggplot2}} implementation is where much of the magic happens, 
#' and, an ideal base-platform for the \code{ggtern} package.
#' 
#' In this document, some of the main features are highlighted, however, current examples (and corresponding outputs) 
#' can be viewed at http://ggtern.com
#'
#' @section \code{ggtern} Constructor:
#' Plots in \code{ggtern} are instigated via the default constructor: \code{ggtern(...)}, 
#' for additional information, click \link[=ggtern]{HERE}:
#' 
#' @section \code{ggtern} Ternary Coordinate System:
#' The foundation of this package, is the ternary coordinate system, which can be produced with the \code{coord_tern(...)} command and added to an existing 
#' ggplot object. The \code{ggtern(...)} constructor adds the \code{coord_tern(...)} coordinate system by default. 
#' For further information on the \code{coord_tern(...)} coordinate system, click \link[=coord_tern]{HERE}.
#'
#' @section  \code{ggtern} Valid Geometries:
#' ggplot2, using the \code{\link{grid}} and \code{\link{proto}} architectures, makes provision for a many number of geometries to be added 
#' progressively in \emph{'layers'} to a given base plot. Due to the nature of the ternary coordinate system, some of the 
#' geometries which are available in ggplot2, are \strong{not relevant} (or won't function) with ternary plots and as such, a limited number of 
#' 'approved' geometries can be used. Click \link[=approved_layers]{HERE} for the full list of approved geometries.
#' 
#' Notably, \code{ggtern} includes novel geometries not available to \code{ggplot2} which include:
#' \enumerate{
#'   \item \code{\link[=geom_confidence]{Confidence Intervals via the Mahalnobis Distance}}
#'   \item \code{\link[=geom_errorbarT]{Ternary Errorbars}}
#'   \item \code{\link[=geom_Tline]{Ternary Constant-Lines}}
#' }
#' 
#' @section  \code{ggtern} Handling Non-Approved Geometries:
#' If a geometric layer is added that is \strong{NOT} contained in the approved \link[=approved_layers]{list}, \strong{IT WILL BE STRIPPED / IGNORED} from the ternary diagram 
#' when rendering takes place (notifying the user to such effect). The reason for this is that subtle 'patches' have been applied, which are mainly to do with 
#' the transformation procedures when incorporating a 'third' dimention. \strong{NB:} In the future, others may be made available once patched.
#'
#' @section  \code{ggtern} New Theme Elements and Heirarchies:
#' \code{ggtern} implements many new theme elements and heirarchies which can be tailored on a case-by-case basis. 
#' The full list of new elements can is provided \link[=theme_elements]{HERE}.
#'
#' @section  \code{ggtern} Theme Element Convenience Functions:
#' \code{ggtern} has made available a number of convenience functions, for rapid tweaking of common theme elements, for a comprehensive list, 
#' see \link[=theme_convenience]{HERE}.
#'
#' @section  \code{ggtern} Modification to Required Aesthetics:
#' Each geometry has a pre-determined set of \strong{required} aesthetics. These have been modifid such that where \code{x} and \code{y} were previously 
#' required, now an additional \code{z} aesthetic is required (\code{geom_segment} now requires \code{z} and \code{zend}). 
#' This is made possible without affecting the standard ggplot2 behaviour because \code{ggtern} distinuishes between \code{\link[=ggplot]{ggplot2}} and 
#' \code{ggtern} objects, distinguished by the presence of the \code{coord_tern(...)} coordinate system.
#' 
#' @section  \code{ggtern} Provided Datasets:
#' \code{ggtern} ships with a number of datasets, including:
#' \enumerate{
#'   \item \code{\link[=data_Feldspar]{Elkin and Groves Feldspar Data}}
#'   \item \code{\link[=data_USDA]{USDA Textural Classification Data}}
#'   \item \code{\link[=data_Fragments]{Grantham and Valbel Rock Fragment Data}}
#' }
#' 
#' @references
#' To cite this package, please use the following:
#' 
#' Hamilton NE and Ferry M (2018). "ggtern: Ternary Diagrams Using ggplot2." 
#' Journal of Statistical Software, Code Snippets, 87(3), pp. 1-17. 
#' doi: 10.18637/jss.v087.c03 (URL:http://doi.org/10.18637/jss.v087.c03)
#' 
#' A bibtex entry can be obtained by executing the following command:
#' \code{citation('ggtern')}
#' 
#' @examples
#' ##-----------------------------------------------
#' ## Basic Usage
#' ##-----------------------------------------------
#' df = data.frame(x = runif(50),
#'                 y = runif(50),
#'                 z = runif(50),
#'                 Value = runif(50,1,10),
#'                 Group = as.factor(round(runif(50,1,2))))
#' ggtern(data=df,aes(x,y,z,color=Group)) + 
#'   theme_rgbw() + 
#'   geom_point() + geom_path() + 
#'   labs(x="X",y="Y",z="Z",title="Title")
#' 
#' @import ggplot2 gtable plyr proto lattice gridExtra
#' @importFrom scales rescale censor squish_infinite expand_range squish fullseq
#' @importFrom latex2exp TeX
#' @importFrom compositions ilr ilrInv clr clrInv acomp
#' @importFrom MASS bandwidth.nrd
#' @importFrom stats dnorm qchisq
#' @importFrom utils getFromNamespace packageVersion
#' @importFrom grid grid.edit pop.viewport grid.path getGrob childNames convertNative grid.polygon grid.yaxis viewport.layout grid.multipanel grid.reorder engine.display.list is.grob grid.move.to placeGrob layout.heights viewport removeGrob grid.gedit layout.widths xsplinePoints absolute.size grid.legend nullGrob makeContent convertX explode convertY plotViewport functionGrob forceGrob arcCurvature grid.abline grid.gremove grid.display.list grid.convert setChildren rasterGrob addGrob vpTree grid.grob xaxisGrob depth grid.locator grid.gget calcStringMetric rectGrob grid.place grid.newpage nestedListing grid.revert grid.grabExpr grid.bezier grid.xaxis gpar yDetails grid.circle gPath grid.panel current.rotation grid.points seekViewport grid.collection current.parent layout.torture pushViewport grobName grid.cap grid.clip grobWidth vpList xDetails grid.copy unit.length current.transform push.viewport resolveRasterSize grid.polyline grid.arrows grid.pretty popViewport grid.convertHeight grid.refresh grid.text grid.convertX grid.convertY applyEdits grid.show.layout stringAscent valid.just grid.strip dataViewport pointsGrob resolveHJust grobAscent reorderGrob gEdit segmentsGrob polylineGrob draw.details grid.frame editGrob editDetails showViewport lineToGrob moveToGrob applyEdit unit.pmin grid.ls grobPathListing grid.pack convertWidth stringWidth linesGrob textGrob grob grid.add grid.delay makeContext grid.curve grid.force arrowsGrob current.vpTree grid.record convertHeight legendGrob grid.set setGrob preDrawDetails bezierPoints grobHeight grid.xspline stringDescent getNames current.viewport pathGrob gTree yaxisGrob convertUnit vpPath grid.plot.and.legend current.vpPath grid.function frameGrob grid.grep viewport.transform ascentDetails grid.remove grid.DLapply grobTree grid.draw downViewport unit.c pathListing packGrob grid.get unit.pmax is.unit curveGrob gEditList grid.grab grid.null grid.layout grid.raster heightDetails showGrob stringHeight widthDetails circleGrob roundrectGrob upViewport vpStack grid.segments layoutRegion gList grobDescent grid.rect unit.rep xsplineGrob grid.roundrect clipGrob grid.lines grid.grill grid.convertWidth drawDetails grid.show.viewport validDetails grid.line.to polygonGrob bezierGrob get.gpar resolveVJust grobX descentDetails grobY postDrawDetails
#' @importFrom methods is 
#' 
#' @author Nicholas Hamilton
#' @aliases ggtern-package
#' @name    ggtern_package
#' @rdname  ggtern_package
NULL
