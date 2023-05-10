#' @title Elkin and Groves Feldspar Data
#' 
#' @description Data relating to Elkins and Groves Feldspar Data, the following datasets include 
#' the experimental data and sample raster data from one of the images in the 
#' referenced paper.
#' \code{Feldspar} - Experimental Data
#' \code{FeldsparRaster} - Raster Data for Fig. 6.
#' 
#' @references 
#' Elkins, L. T. & Grove, T. L. 
#' Ternary Feldspar Experiments and Thermodynamic Models 
#' American Mineralogist, Mineral Soc America, 1990, 75, 544-559
#' @docType data
#' @usage 
#' #Experimental Data
#' data(Feldspar)
#' 
#' #Raster data
#' data(FeldsparRaster)
#' @format 
#' \code{Feldsdpar} - One (1) row per Feldspar composition, \code{FeldsdparRaster} - Raster Matrix
#' @examples
#' #Summarize the Feldspar Data
#' data(Feldspar)
#' summary(Feldspar)
#' 
#' #Plot Felspar Data
#' ggtern(data=Feldspar,aes(x=An,y=Ab,z=Or)) + 
#'     geom_point()
#' 
#' # Plot Feldspar data and Underlying Raster Image
#' data(FeldsparRaster)
#' ggtern(Feldspar,aes(Ab,An,Or)) + 
#'     theme_rgbw() + 
#'     annotation_raster_tern(FeldsparRaster,xmin=0,xmax=1,ymin=0,ymax=1) +
#'     geom_point(size=5,aes(shape=Feldspar,fill=Feldspar),color='black') +
#'     scale_shape_manual(values=c(21,24)) +
#'     labs(title = "Demonstration of Raster Annotation")
#'     
#' @seealso \link[=data]{Data}
#' @name data_Feldspar
#' @rdname data_Feldspar
#' @aliases Feldspar FeldsparRaster
#' @author Nicholas Hamilton
NULL

#' @title USDA Textural Classification Data
#' 
#' @description This dataset was issued by the United States Department of Agriculture (USDA) 
#' in the form of a ternary diagram, this original ternary diagram has been converted to numerical data 
#' and included here.
#' @docType data
#' @usage data(USDA)
#' @format 1row per point, many points per classification representing the extremes of the area.
#' @source Soil Mechanics Level 1, Module 3, USDA Textural Classification Study Guide
#' @author United States Department of Agriculture (USDA)
#' @seealso \link[=data]{ggtern datasets}
#' @examples
#' #Load the Libraries
#' library(ggtern)
#' library(plyr)
#' 
#' #Load the Data.
#' data(USDA)
#' 
#' #Put tile labels at the midpoint of each tile.
#' USDA.LAB <- ddply(USDA,"Label",function(df){
#'   apply(df[,1:3],2,mean)
#' })
#' 
#' #Tweak
#' USDA.LAB$Angle = sapply(as.character(USDA.LAB$Label),function(x){
#'     switch(x,"Loamy Sand"=-35,0)
#' })
#' 
#' #Construct the plot.
#' ggtern(data=USDA,aes(Sand,Clay,Silt,color=Label,fill=Label)) +
#'   geom_polygon(alpha=0.75,size=0.5,color="black") +
#'   geom_mask() +  
#'   geom_text(data=USDA.LAB,aes(label=Label,angle=Angle),color="black",size=3.5) +
#'   theme_rgbw() + 
#'   theme_showsecondary() +
#'   theme_showarrows() +
#'   weight_percent() + 
#'   guides(fill='none') + 
#'   theme_legend_position("topleft") + 
#'   labs(title = "USDA Textural Classification Chart",
#'        fill  = "Textural Class",
#'        color = "Textural Class")
#' @name data_USDA
#' @rdname data_USDA
#' @aliases USDA
#' @author Nicholas Hamilton
NULL



#' Grantham and Valbel Rock Fragment Data
#' 
#' \strong{ABSTRACT:} Chemical weathering influences the detrital composition of sand-size sediment derived from source 
#' areas subject to different amounts of precipitation in the Coweeta Basin, North Carolina. Of the grain types 
#' studied, rock fragments are most sensitive to chemical degradation; therefore, their abundance is the best 
#' indicator of cumulative weathering effects. Destruction of sand-size rock fragments by chemical weathering 
#' is a function of both the intensity and duration of chemical weathering experienced by grains in regoliths 
#' of the source area. In the Coweeta Basin, the intensity of chemical weathering is directly related to the 
#' climate via effective precipitation in individual subbasins, whereas the duration of chemical weathering is 
#' inversely related to the relief ratio of the watershe . Therefore, soils in watersheds with low-relief 
#' ratios and high discharge per unit area experience the most extensive chemical weathering, and sediments 
#' derived from these watersheds contain the lowest percentage of rock fragments. The effects of climate alone 
#' cannot explain the systematic variation of rock fragment abundance in sediments from the Coweeta Basin. 
#' The compositional imprint left on these sediments by chemical weathering is a function of both climate and 
#' topographic slope in the sediment source area.
#' @docType data
#' @references Grantham, Jeremy Hummon, and Michael Anthony Velbel. 
#' "The influence of climate and topography on rock-fragment abundance in modern fluvial sands of the southern 
#' Blue Ridge Mountains, North Carolina." Journal of Sedimentary Research 58.2 (1988).
#' @usage data(Fragments)
#' @format 1row per point, Each point contains data on the following:
#' \enumerate{ 
#' \item \strong{Watershed}: By id: 2, 10, 34, 41, 13, 27, 32 or 37, 
#' \item \strong{Position}: By name: Tallulah or Coweeta, 
#' \item \strong{CCWI}: The Cumulative Chemical Weathering Index: numeric
#' \item \strong{Precipitation}: Average Annual Precipitation, numeric 
#' \item \strong{Discharge}: Annual Average Discharge, numeric 
#' \item \strong{Relief}: Relief Ratio, numeric
#' \item \strong{GrainSize}: Coarse Medium or Fine, 
#' \item \strong{Sample}: Field Sampling, A, B or C 
#' \item \strong{Points}: The number of points measured for each sample
#' \item \strong{Qm}: Multicrystalline Quarts Amount, percentage
#' \item \strong{Qp}: Polycrystalline Quarts Amount, percentage
#' \item \strong{Rf}: Rock Fragments Amount, percentage
#' \item \strong{M}: Mica Amount, percentage
#' }
#' @name data_Fragments
#' @rdname data_Fragments
#' @aliases Fragments
#' @author Jeremy Hummon Grantham and Michael Anthony Velbel
#' @examples 
#' data(Fragments)
#' ggtern(Fragments,aes(Qm+Qp,Rf,M,colour=Sample)) +
#'   geom_density_tern(h=2,aes(fill=..level..),
#'   expand=0.75,alpha=0.5,bins=5) + 
#'   geom_point(aes(shape=Position,size=Relief)) + 
#'   theme_bw(base_size=8) + 
#'   theme_showarrows() + 
#'   custom_percent('%') + 
#'   labs(title = "Grantham and Valbel Rock Fragment Data",
#'        x = "Q_{m+p}", xarrow = "Quartz (Multi + Poly)",
#'        y = "R_f",     yarrow = "Rock Fragments",
#'        z = "M",       zarrow = "Mica") + 
#'   theme_latex() + 
#'   facet_wrap(~Sample,nrow=2)
NULL


#' Aichisons Skye Lavas
#' 
#' AFM compositions of 23 aphyric Skye lavas.
#' 
#' @docType data
#' @references Aitchison, J. 
#'             The statistical analysis of compositional data 
#'             Chapman and Hall London, 1986, pp360
#' @name data_SkyeLava
#' @rdname data_SkyeLava
#' @aliases SkyeLava
#' @author J. Aitchison
#' @format 1 row per point, 23 points in total, Each point contains data on the following:
#' \enumerate{ 
#' \item \strong{No}: ID, S1 to S23
#' \item \strong{A}: Percent Na2O+K2O , 
#' \item \strong{F}: Percent Fe2O3
#' \item \strong{F}: Percent MgO
#' }
#' @examples 
#' 
#' # Emulate & Enhance plot produced in Fig. 3, pg 7 of:
#' # Martin-Fernandez, J.; Chacon-Duran, J. & Mateu-Figueras, G.
#' # Updating on the kernel density estimation for compositional data 
#' # Proceedings of 17th Conference IASC-ERSS, Compstat, Roma,(Italy), 2006, 713-720
#' 
#' data(SkyeLava)
#' breaks  = c(.01,.05,.10,.25,.5,.75,.9,.95,.99)
#' ggtern(SkyeLava,aes(F,A,M)) + 
#' theme_bw() + 
#' theme_showarrows() + 
#' theme_latex() + 
#' theme(tern.panel.grid.minor = element_blank(),
#'       tern.panel.grid.major = element_line(linetype='dotted',color='darkgray'),
#'       tern.axis.text        = element_text(size=8)) + 
#'       geom_density_tern() + 
#'       geom_point() +
#'       limit_tern(breaks = breaks,
#'                  labels = sprintf("%.2f",breaks)) +
#' labs(title    = "Aphyric Skye Lavas",
#'      subtitle = "AFM Compositions of 23 samples",
#'      Tarrow = "A = Na_2O + K_2O",
#'      Larrow = "F = Fe_20_3",
#'      Rarrow = "M = MgO")
NULL

#' Aichisons White Cells
#' 
#' White-cell compositions of 30 blood cells by two different methods
#' @docType data
#' @references Aitchison, J. 
#'             The statistical analysis of compositional data 
#'             Chapman and Hall London, 1986, pp366
#' @name data_WhiteCells
#' @rdname data_WhiteCells
#' @aliases WhiteCells
#' @author J. Aitchison
#' @format 1 row per point, 60 points in total, 2 experiments x 30 points each, Each point contains data on the following:
#' \enumerate{ 
#' \item \strong{No}: ID, S1 to S30
#' \item \strong{Experiment}: MicroscopicInspection or ImageAnalysis
#' \item \strong{G}: Fraction Granulocytes
#' \item \strong{L}: Fraction Lymphocytes
#' \item \strong{M}: Fraction Monocytes
#' }
#' @examples
#' data(WhiteCells)
#'    ggtern(WhiteCells,aes(G,L,M)) + 
#'    geom_density_tern(aes(color=Experiment)) +
#'    geom_point(aes(shape=Experiment)) +
#'    facet_wrap(~Experiment,nrow=2)
NULL