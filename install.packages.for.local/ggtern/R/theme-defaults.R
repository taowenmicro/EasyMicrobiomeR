#' List of Available Themes
#' 
#' \code{ggtern} ships with a number of complete themes, summarized as follows. 
#' These themes combine the base themes available to \code{ggplot2} and a number of NEW themes, which
#' are unique to \code{ggtern}.
#' \itemize{
#'  \item{ Black and White Theme:}{ 
#'    \code{\link[=theme_bw]{theme_bw(...)}}
#'  }
#'  \item{Minimal Theme:}{
#'    \code{\link[=theme_minimal]{theme_minimal(...)}}
#'  }
#'  \item{Classic Theme:}{
#'    \code{\link[=theme_classic]{theme_classic(...)}}
#'  }
#'  \item{Gray and White Theme:}{
#'    \code{\link[=theme_gray]{theme_gray(...)}}
#'  }
#'  \item{Red, Green, Blue and White Theme:}{
#'    \code{\link[=theme_rgbw]{theme_rgbw(...)}}
#'  }
#'  \item{Red, Green, Blue and Gray Theme:}{
#'    \code{\link[=theme_rgbg]{theme_rgbg(...)}}
#'  }
#'  \item{Dark Theme:}{
#'    \code{\link[=theme_dark]{theme_dark(...)}}
#'  }
#'  \item{Darker Theme:}{
#'    \code{\link[=theme_darker]{theme_darker(...)}}
#'  }
#'  \item{Light Theme:}{
#'    \code{\link[=theme_light]{theme_light(...)}}
#'  }
#'  \item{Theme with Only Black Lines:}{
#'    \code{\link[=theme_linedraw]{theme_linedraw(...)}}
#'  }
#'  \item{Matrix Theme:}{
#'    \code{\link[=theme_matrix]{theme_matrix(...)}}
#'  }
#'  \item{Tropical Theme:}{
#'    \code{\link[=theme_tropical]{theme_tropical(...)}}
#'  }
#'  \item{BlueLight Theme:}{
#'    \code{\link[=theme_bluelight]{theme_bluelight(...)}}
#'  }
#'  \item{BlueDark Theme:}{
#'    \code{\link[=theme_bluedark]{theme_bluedark(...)}}
#'  }
#'  \item{Black Vermillion Blue Theme (White Background):}{
#'    \code{\link[=theme_bvbw]{theme_bvbw(...)}}
#'  }
#'  \item{Black Vermillion Blue Theme (Grey Background):}{
#'    \code{\link[=theme_bvbg]{theme_bvbg(...)}}
#'  }
#' }
#' @seealso \code{\link{ggtern_themes}}
#' @author Nicholas Hamilton
#' @rdname theme_complete
#' @name theme_complete
NULL

#' ggtern themes
#'
#' Themes set the general aspect of the plot such as the colour of the
#' background, gridlines, the size and colour of fonts.
#'
#' @param base_size base font size
#' @param base_family base font family
#'
#' @details \describe{
#'
#' \item{\code{theme_gray}}{
#' The signature ggplot2 theme with a grey background and white gridlines,
#' designed to put the data forward yet make comparisons easy.
#' }
#'
#' \item{\code{theme_bw}}{
#' The classic dark-on-light ggplot2 theme. May work better for presentations
#' displayed with a projector.
#' }
#'
#' \item{\code{theme_linedraw}}{
#' A theme with only black lines of various widths on white backgrounds,
#' reminiscent of a line drawings. Serves a purpose similar to \code{theme_bw}.
#' Note that this theme has some very thin lines (<< 1 pt) which some journals
#' may refuse.}
#'
#' \item{\code{theme_light}}{
#' A theme similar to \code{theme_linedraw} but with light grey lines and axes,
#' to direct more attention towards the data.
#' }
#'
#' \item{\code{theme_dark}}{
#' The dark cousin of \code{theme_light}, with similar line sizes but a dark background. 
#' Useful to make thin coloured lines pop out.
#' }
#'
#' \item{\code{theme_darker}}{
#' A darker cousing to \code{theme_dark}, with a dark panel background.
#' }
#'
#' \item{\code{theme_minimal}}{
#' A minimalistic theme with no background annotations.
#' }
#'
#' \item{\code{theme_classic}}{
#' A classic-looking theme, with x and y axis lines and no gridlines.
#' }
#' 
#' \item{\code{theme_rgbw}}{
#'  A theme with white background, red, green and blue axes and gridlines
#' }
#' 
#' \item{\code{theme_rgbg}}{
#' A theme with grey background, red, green and blue axes and gridlines
#' }
#'
#' \item{\code{theme_void}}{ 
#' A completely empty theme.
#' }
#' 
#' \item{\code{theme_custom}}{
#' Theme with custom basic colours
#' }
#' 
#' \item{\code{theme_matrix}}{
#' Theme with very dark background and bright green features
#' }
#' 
#' \item{\code{theme_tropical}}{
#' Theme with tropical colours
#' }
#' 
#' \item{\code{theme_bluelight}}{
#' A blue theme with light background and dark features
#' }
#' 
#' \item{\code{theme_bluedark}}{
#' A blue theme with dark background and light features
#' }
#' 
#' \item{\code{theme_bvbw}}{
#' A black/vermillion/blue theme with white background, for colorblind sensitive readers, see references.
#' }
#' 
#' \item{\code{theme_bvbg}}{
#' A black/vermillion/blue theme with grey background, for colorblind sensitive readers, see references.
#' }
#' 
#' }
#'
#' @examples
#' \donttest{
#' #Create a list of the theme suffixes
#' themesOrg = c('gray','bw','linedraw','light',
#'               'dark','minimal','classic','void')
#' themesNew = c('custom','darker','rgbw','rgbg','tropical',
#'               'matrix','bluelight','bluedark','bvbw','bvbg')
#' 
#' #Iterate over all the suffixes, creating a list of plots
#' plotThemes = function(themes){
#'    grobs = lapply(themes,function(x){
#'      thmName = sprintf("theme_%s",x)
#'      thm = do.call(thmName,args=list(base_size=9))
#'      df  = data.frame(label=thmName)
#'      ggtern(df) + facet_wrap(~label) + thm
#'    })
#'    grobs
#' }
#' 
#' #Arrange the Original Themes
#' grid.arrange(grobs=plotThemes(themesOrg),top = "Collection of Themes (Original)")
#' 
#' #Arrange the New Themes
#' grid.arrange(grobs=plotThemes(themesNew),top = "Collection of Themes (New Themes)")
#' }
#' 
#' @references 
#' Okabe, Masataka, and Kei Ito. "How to make figures and presentations that are friendly to color blind people." 
#' University of Tokyo (2002). http://jfly.iam.u-tokyo.ac.jp/color/
#' 
#' @author Nicholas Hamilton
#' @name ggtern_themes
#' @rdname ggtern_themes
NULL

#' @rdname ggtern_themes
#' @export
theme_ggtern <- function(base_size = 11, base_family = ""){
  #Base ggplot2 theme
  baseTheme = get("theme_gray",asNamespace("ggplot2"))
  
  #Create Instance of the base theme
  base = baseTheme(base_size = base_size, base_family = base_family)
  
  #Start with the base theme 
  base %+replace%
  
  #Start with additional elements
  theme(
    
    ##TERNARY PANEL
    tern.panel.background          = element_rect(),  #Panel is the triangular region
    tern.plot.background           = element_rect(), #Plot  is the rectangular outer region
    tern.plot.latex                = getOption('tern.latex'), #Parse Labels as Latex
    
    ##AXIS ARROWS
    #tern.axis                     = element_line(),
    tern.axis.hshift               = getOption("tern.hshift"),
    tern.axis.vshift               = getOption("tern.vshift"),
    tern.axis.clockwise            = getOption("tern.clockwise"),
    
    tern.axis.line                 = element_line(),
    tern.axis.line.T               = element_line(),
    tern.axis.line.L               = element_line(),
    tern.axis.line.R               = element_line(),
    tern.axis.line.ontop           = getOption("tern.line.ontop"),
    
    #Axis Titles
    tern.axis.title                = element_text(),
    tern.axis.title.T              = element_text(),
    tern.axis.title.L              = element_text(),
    tern.axis.title.R              = element_text(),
    tern.axis.title.show           = getOption("tern.title.show"), 
    
    #Axis Text
    tern.axis.text                 = element_text(),
    tern.axis.text.T               = element_text(),
    tern.axis.text.L               = element_text(),
    tern.axis.text.R               = element_text(),
    tern.axis.text.show            = getOption("tern.text.show"),
    
    #Arrow
    tern.axis.arrow                = element_line(lineend = getOption('tern.arrow')),
    tern.axis.arrow.T              = element_line(),
    tern.axis.arrow.L              = element_line(),
    tern.axis.arrow.R              = element_line(),
    tern.axis.arrow.text           = element_text(),
    tern.axis.arrow.text.T         = element_text(),
    tern.axis.arrow.text.L         = element_text(),
    tern.axis.arrow.text.R         = element_text(),
    tern.axis.arrow.sep            = getOption("tern.arrow.sep"),
    tern.axis.arrow.show           = getOption("tern.arrow.show"),
    tern.axis.arrow.start          = getOption("tern.arrow.start"),
    tern.axis.arrow.finish         = getOption("tern.arrow.finish"),
    
    #Ticks
    tern.axis.ticks                = element_line(),
    tern.axis.ticks.major          = element_line(),
    tern.axis.ticks.major.T        = element_line(),
    tern.axis.ticks.major.L        = element_line(),
    tern.axis.ticks.major.R        = element_line(),
    tern.axis.ticks.length.major   = 1.0*base$axis.ticks.length,
    tern.axis.ticks.length.minor   = 0.5*base$axis.ticks.length,
    tern.axis.ticks.outside        = getOption("tern.ticks.outside"),
    tern.axis.ticks.primary.show   = getOption("tern.ticks.primary.show"),
    tern.axis.ticks.secondary.show = getOption("tern.ticks.secondary.show"),
    tern.axis.ticks.minor          = element_line(),
    tern.axis.ticks.minor.T        = element_line(),
    tern.axis.ticks.minor.L        = element_line(),
    tern.axis.ticks.minor.R        = element_line(),
    
    #Panel Grids
    #tern.panel.grid                = element_line(),
    tern.panel.grid.major          = element_line(),
    tern.panel.grid.major.T        = element_line(),
    tern.panel.grid.major.L        = element_line(),
    tern.panel.grid.major.R        = element_line(),
    tern.panel.grid.major.show     = getOption("tern.grid.major.show"),
    tern.panel.grid.minor          = element_line(),
    tern.panel.grid.minor.T        = element_line(),
    tern.panel.grid.minor.L        = element_line(),
    tern.panel.grid.minor.R        = element_line(),
    tern.panel.grid.minor.show     = getOption("tern.grid.minor.show"),
    tern.panel.grid.ontop          = getOption("tern.grid.ontop"),
    tern.panel.mask.show           = getOption("tern.mask.show"),
    tern.panel.expand              = getOption('tern.expand'),
    tern.panel.rotate              = getOption('tern.rotate')
  )
}

#' @rdname ggtern_themes
#' @export
theme_gray  <- function(base_size = 11, base_family = ""){
  base = theme_ggtern(base_size = base_size, base_family = base_family) 
  base %+replace%
  theme(
    tern.plot.background  = element_rect(size=NA,color=NA),
    tern.axis.arrow       = element_line(
      color               = calc_element('axis.text',base)$colour,
      lineend             = getOption('tern.arrow')
    ),
    tern.axis.line.T    = element_blank(),
    tern.axis.line.L    = element_blank(),
    tern.axis.line.R    = element_blank()
  )
}
theme_grey <- theme_gray

#' @rdname ggtern_themes
#' @export
theme_bw <- function(base_size = 12, base_family = "") {
  base = ggplot2::theme_bw(base_size,base_family)
  theme_ggtern(base_size,base_family) %+replace% 
  base %+replace%
  theme(
    tern.plot.background  = element_rect(size=NA,color=NA),
    tern.axis.line        = element_line(color  = base$panel.border$colour),
    tern.axis.arrow       = element_line(
      color               = base$panel.border$colour,
      lineend             = getOption('tern.arrow')
    )
  )
}

#' @rdname ggtern_themes
#' @export
theme_linedraw <- function(base_size = 12, base_family = "") {
  base = ggplot2::theme_linedraw(base_size, base_family)
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    base %+replace%
    theme(
      tern.axis.line    = element_line(color = base$panel.border$colour,
                                       size  = base$panel.border$size/2),
      tern.axis.ticks.minor = element_blank(),
      tern.axis.arrow       = element_line(
        color               = base$panel.border$colour,
        lineend             = getOption('tern.arrow')
      )
    )
}

#' @rdname ggtern_themes
#' @export
theme_light <- function(base_size = 12, base_family = "") {
  base     = ggplot2::theme_light(base_size, base_family) 
  baseline = element_line(color = base$panel.border$colour,
                          size  = base$panel.border$size/2)
  theme_ggtern() %+replace%
    base %+replace%
    theme(
      tern.plot.background  = element_rect(size=NA,color=NA),
      tern.axis.line        = baseline,
      tern.axis.ticks       = base$axis.ticks,
      tern.axis.ticks.minor = element_blank(),
      tern.axis.arrow       = element_line(
        color               = baseline$colour,
        size                = baseline$size,
        lineend             = getOption('tern.arrow')
      )
    )
}

#' @rdname ggtern_themes
#' @export
theme_minimal <- function(base_size = 12, base_family = "") {
  theme_bw(base_size,base_family) %+replace%
  ggplot2::theme_minimal(base_size,base_family) %+replace%
  theme(
    tern.axis.line        = element_line(),
    tern.axis.line.T      = element_blank(),
    tern.axis.line.L      = element_blank(),
    tern.axis.line.R      = element_blank(),
    tern.axis.ticks.major = element_blank(),
    tern.axis.ticks.minor = element_blank(),
    tern.axis.arrow       = element_blank(),
    tern.axis.arrow.text  = element_blank()
  )
}

#' @rdname ggtern_themes
#' @export
theme_classic <- function(base_size = 12, base_family = ""){
  base = ggplot2::theme_classic(base_size, base_family)
  
  theme_bw(base_size, base_family) %+replace%
    base %+replace%
    theme(
      tern.axis.line          = element_blank(),
      tern.panel.grid.major   = element_line(),
      tern.panel.grid.major.T = element_blank(),
      tern.panel.grid.major.L = element_blank(),
      tern.panel.grid.major.R = element_blank(),
      tern.panel.grid.minor   = element_line(),
      tern.panel.grid.minor.T = element_blank(),
      tern.panel.grid.minor.L = element_blank(),
      tern.panel.grid.minor.R = element_blank(),
      tern.axis.ticks.minor   = element_blank()
    )
}

#' @rdname ggtern_themes
#' @export
theme_dark <- function(base_size = 12, base_family = "") {
  base = ggplot2::theme_dark(base_size,base_family)
  theme_ggtern(base_size,base_family) %+replace% 
  base %+replace%
  theme(
    tern.plot.background  = element_rect(size=NA,color=NA),
    tern.axis.line  = element_line( colour = "grey40", size = 0.25),
    tern.axis.ticks = element_line( colour = "grey40", size = 0.25),
    tern.axis.ticks.minor = element_blank()
  )
}

#' @rdname ggtern_themes
#' @export
theme_void <- function(base_size = 12, base_family = "") {
  theme_ggtern(base_size,base_family) %+replace%
  ggplot2::theme_void(base_size,base_family) %+replace%
  theme(
    #text                 = element_blank(),
    #line                 = element_blank(),
    #rect                 = element_blank(),
    tern.axis.text       = element_blank(),
    tern.axis.title      = element_blank()
  )
}

#-------------------------------------------------------------
# CUSTOM THEMES
#-------------------------------------------------------------

#' @rdname ggtern_themes
#' @export
theme_darker <- function(base_size = 12, base_family = "") {
  base = theme_dark(base_size,base_family)
  base %+replace%
  theme(plot.background       = element_rect(fill='grey75',colour=NA),
        legend.background     = element_rect(fill='grey50',colour=NA))
}

#' @param col.T colour of top axis, ticks labels and major gridlines
#' @param col.L colour of left axis, ticks, labels and major gridlines
#' @param col.R colour of right axis, ticks, labels and major gridlines
#' @param tern.plot.background colour of background colour to plot area
#' @param tern.panel.background colour of panel background of plot area
#' @param col.grid.minor the colour of the minor grid
#' \code{theme_custom} is a convenience function to allow the user to control the basic theme colours very easily.
#' @rdname ggtern_themes
#' @export
theme_custom <- function(base_size             = 12,
                         base_family           = "",
                         tern.plot.background  = NULL,
                         tern.panel.background = NULL,
                         col.T                 = 'black',
                         col.L                 = 'black',
                         col.R                 = 'black',
                         col.grid.minor        = "white"){
  
  #Start with the base theme, then replace some elements
  theme_ggtern(base_size, base_family) %+replace%
  
  #Elements to replace
  theme(
    tern.plot.background    = element_rect(fill  = tern.plot.background,size=NA,color=NA),
    tern.panel.background   = element_rect(fill  = tern.panel.background),
    tern.panel.grid.major   = element_line(linetype=6,size=0.50),
    tern.panel.grid.major.T = element_line(color = col.T),
    tern.panel.grid.major.L = element_line(color = col.L),
    tern.panel.grid.major.R = element_line(color = col.R),
    tern.panel.grid.minor   = element_line(linetype=1,size=0.25,colour=col.grid.minor),
    tern.axis.title.T       = element_text(color = col.T),
    tern.axis.title.L       = element_text(color = col.L),
    tern.axis.title.R       = element_text(color = col.R),
    tern.axis.text.T        = element_text(color = col.T),
    tern.axis.text.L        = element_text(color = col.L),
    tern.axis.text.R        = element_text(color = col.R),
    tern.axis.arrow.T       = element_line(color = col.T),
    tern.axis.arrow.L       = element_line(color = col.L),
    tern.axis.arrow.R       = element_line(color = col.R),
    tern.axis.arrow.text.T  = element_text(color = col.T),
    tern.axis.arrow.text.L  = element_text(color = col.L),
    tern.axis.arrow.text.R  = element_text(color = col.R),
    tern.axis.line.T        = element_line(color = col.T),
    tern.axis.line.L        = element_line(color = col.L),
    tern.axis.line.R        = element_line(color = col.R),
    tern.axis.ticks.major.T = element_line(color = col.T),
    tern.axis.ticks.major.L = element_line(color = col.L),
    tern.axis.ticks.major.R = element_line(color = col.R),
    tern.axis.ticks.minor.T = element_line(color = col.T),
    tern.axis.ticks.minor.L = element_line(color = col.L),
    tern.axis.ticks.minor.R = element_line(color = col.R)
  )
}

#' @rdname ggtern_themes
#' @export
theme_rgbw  <- function(base_size = 12, base_family = ""){
  #Start with grey theme, then overwrite with custom theme
  #theme_grey(base_size, base_family) %+replace%
  theme_custom(base_size,base_family,col.T='darkred',col.L='darkblue',col.R='darkgreen') +
    
  #Add, not replace
  theme(
    tern.panel.background  = element_rect(fill  = 'white'),
    tern.panel.grid.minor  = element_line(color = "gray90"),
    tern.axis.arrow.show   = TRUE
  )
}

#' @rdname ggtern_themes
#' @export
theme_rgbg <- function(base_size = 12, base_family = ""){
  
  #Start with rgbw theme, and modify slightly
  theme_rgbw(base_size, base_family) + 
    
  #Add to, not replace
  theme(
    tern.panel.background  = element_rect(fill  = 'gray92'),
    tern.panel.grid.minor  = element_line(color = "white")
  )
}
theme_rgb <- theme_rgbg

#' @rdname ggtern_themes
#' @export
theme_matrix = function(base_size = 12, base_family = ""){
    featA = 'green'; featAG = 'darkgreen'; featB = NA; featC = 'grey15'; featD = 'grey10'; featE = 'grey15'
    theme_custom(base_size             = base_size, 
                 base_family           = base_family, 
                 tern.plot.background  = NULL, #FORCE INHERIT
                 tern.panel.background = featC,
                 col.T                 = featA,
                 col.L                 = featA,
                 col.R                 = featA,
                 col.grid.minor        = featB) +
    theme(text                    = element_text(color = featA),
          plot.background         = element_rect(fill  = featD),
          strip.background        = element_rect(color = featA, fill = featE),
          strip.text              = element_text(color = featA),
          tern.panel.grid.major.T = element_line(color = featAG),
          tern.panel.grid.major.L = element_line(color = featAG),
          tern.panel.grid.major.R = element_line(color = featAG),
          tern.axis.arrow.show    = TRUE)
}

#' @rdname ggtern_themes
#' @export
theme_tropical = function(base_size = 12, base_family = ""){
  col.T = '#fe8f0f'; col.L = '#f7325e'; col.R = '#7dc410'; col.text  = '#0264ed'; 
  col.bg.strip = 'gray90'; col.bg = 'white'; 
  theme_custom(base_size             = base_size, 
               base_family           = base_family, 
               tern.plot.background  = NULL, 
               tern.panel.background = col.bg,
               col.T                 = col.T,
               col.L                 = col.L,
               col.R                 = col.R,
               col.grid.minor        = col.bg.strip) +
    theme(text                       = element_text(color = col.text),
          strip.background           = element_rect(color = col.text, fill = col.bg.strip),
          strip.text                 = element_text(color = col.text),
          tern.axis.arrow.show       = TRUE)
}

#' @rdname ggtern_themes
#' @export
theme_bluedark = function(base_size = 12, base_family = ""){
  featA = 'white'; featAG = 'white'; featB = NA; 
  featC = '#000080'; featD = '#000040'; featE = '#000060'
  theme_custom(base_size             = base_size, 
               base_family           = base_family, 
               tern.plot.background  = NULL, #FORCE INHERIT
               tern.panel.background = featC,
               col.T                 = featA,
               col.L                 = featA,
               col.R                 = featA,
               col.grid.minor        = featB) +
    theme(text                    = element_text(color = featA),
          plot.background         = element_rect(fill  = featD),
          strip.background        = element_rect(color = featA, fill = featE, size=1),
          strip.text              = element_text(color = featA),
          tern.panel.grid.major.T = element_line(color = featAG),
          tern.panel.grid.major.L = element_line(color = featAG),
          tern.panel.grid.major.R = element_line(color = featAG),
          tern.axis.arrow.show    = TRUE)
}

#' @rdname ggtern_themes
#' @export
theme_bluelight = function(base_size = 12, base_family = ""){
  featA = '#000040'; featAG = 'white'; featB = NA; 
  featC = '#000080'; featD = 'white'; featE = 'white'
  theme_custom(base_size             = base_size, 
               base_family           = base_family, 
               tern.plot.background  = NULL, #FORCE INHERIT
               tern.panel.background = featC,
               col.T                 = featA,
               col.L                 = featA,
               col.R                 = featA,
               col.grid.minor        = featB) +
    theme(text                    = element_text(color = featA),
          plot.background         = element_rect(fill  = featD),
          strip.background        = element_rect(color = featC, fill = featE, size=1),
          strip.text              = element_text(color = featC),
          tern.panel.grid.major.T = element_line(color = featAG),
          tern.panel.grid.major.L = element_line(color = featAG),
          tern.panel.grid.major.R = element_line(color = featAG),
          tern.axis.arrow.show    = TRUE)
}


#' @rdname ggtern_themes
#' @export
theme_bvbw  <- function(base_size = 12, base_family = ""){
  #Start with grey theme, then overwrite with custom theme
  col.T = rgb2hex(0,0,0)
  col.L = rgb2hex(213,94,0)
  col.R = rgb2hex(0,114,178)
  theme_custom(base_size,base_family,col.T=col.T,col.L=col.L,col.R=col.R) +
    
    #Add, not replace
    theme(
      tern.panel.background  = element_rect(fill  = 'white'),
      tern.panel.grid.minor  = element_line(color = "gray90"),
      tern.axis.arrow.show   = TRUE
    )
}

#' @rdname ggtern_themes
#' @export
theme_bvbg <- function(base_size = 12, base_family = ""){
  
  #Start with theme_bvbw  theme, and modify slightly
  theme_bvbw(base_size, base_family) + 
    
    #Add to, not replace
    theme(
      tern.panel.background  = element_rect(fill  = 'gray92'),
      tern.panel.grid.minor  = element_line(color = "white")
    )
}

#-------------------------------------------------------------
# REDUNDANT THEMES
#-------------------------------------------------------------
theme_tern_bw <- function(base_size = 12, base_family = ""){
  tern_dep("1.0.1.3","theme_tern_bw has been superceded by the ggplot2 standard theme_bw")
  theme_bw(base_size,base_family)
}
theme_tern_gray <- function(base_size = 12, base_family = ""){
  tern_dep("1.0.1.3","theme_tern_gray has been superceded by the ggplot2 standard theme_gray")
  theme_gray(base_size,base_family)
}
theme_tern_classic <- function(base_size = 12, base_family = ""){
  tern_dep("1.0.1.3","theme_tern_classic has been superceded by the ggplot2 standard theme_classic")
  theme_classic(base_size,base_family)
}
theme_tern_rgbw <- function(base_size = 12, base_family = ""){
  tern_dep("1.0.1.3","theme_tern_rgbw has been superceded by theme_rgbw")
  theme_rgbw(base_size,base_family)
}
theme_tern_rgbg <- function(base_size = 12, base_family = ""){
  tern_dep("1.0.1.3","theme_tern_rgbg has been superceded by theme_rgbg")
  theme_rgbg(base_size,base_family)
}
