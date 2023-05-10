#' New Theme Elements
#' 
#' \code{ggtern} creates many new theme elements and inheritances, the following is an outline:
#'
#' Theme elements can inherit properties from other theme elements.
#' For example, \code{axis.title.x} inherits from \code{axis.title}, 
#' which in turn inherits from \code{text}. All text elements inherit
#' directly or indirectly from \code{text}; all lines inherit from
#' \code{line}, and all rectangular objects inherit from \code{rect}.
#'
#' Modifying the newly created items requires the same procedures as introduced in the ggplot2 \code{\link[ggplot2]{theme}} documentation.
#' Some convenience functions have been also newly created, proceed to \code{\link{theme_convenience_functions}} for additional information.
#' 
#' @section New/Additional Inheritance Structures:
#' \Sexpr[results=rd,stage=build]{ggtern:::rd_theme()}
#' **  \strong{NB:} \code{tern.panel.background}, whilst the ternary area is 'triangular' per-se, \code{\link{element_rect}} has been used, 
#' as it actually holds NO information regarding the geometry (width, height), only fill, color, 
#' size and linetype border (ie the style of how it will be rendered).
#' @author Nicholas Hamilton
#' @rdname theme_elements
#' @name theme_elements
NULL


# Given a theme object and element name, return a grob for the element
element_render <- function(theme, element, ..., name = NULL) {
  # Get the element from the theme, calculating inheritance
  el <- calc_element(element, theme)
  if (is.null(el)) {
    message("Theme element ", element, " missing")
    return(zeroGrob())
  }
  ggint$ggname(paste(element, name, sep = "."), element_grob(el, ...))
}


#SEARCH FOR THE ORIGINAL FUNCTIONS
# ggint$.element_tree     <- find_global_tern(".element_tree")
# ggint$.element_tree.orig <- ggint$.element_tree #To determine the new set relative to the existing.
# .el_def                  <- ggint$el_def
ggtern_register_theme_elements = function(){
  register_theme_elements(
    ##TERNARY PANEL
    tern.panel.background          = element_rect(),
    tern.plot.background           = element_rect(),
    tern.plot.latex                = getOption('tern.latex'),
    
    ##AXIS ARROWS
    tern.axis.hshift               = getOption("tern.hshift"),
    tern.axis.vshift               = getOption("tern.vshift"),
    tern.axis.clockwise            = getOption("tern.clockwise"),
    
    tern.axis.line                 = element_line(),
    tern.axis.line.T               = element_line(),
    tern.axis.line.L               = element_line(),
    tern.axis.line.R               = element_line(),
    tern.axis.line.ontop           = getOption("tern.line.ontop"),
    
    #Axis Text
    tern.axis.text                 = element_text(),
    tern.axis.text.T               = element_text(),
    tern.axis.text.L               = element_text(),
    tern.axis.text.R               = element_text(),
    tern.axis.text.show            = getOption("tern.text.show"),
    
    
    #Axis Titles
    tern.axis.title                = element_text(),
    tern.axis.title.T              = element_text(),
    tern.axis.title.L              = element_text(),
    tern.axis.title.R              = element_text(),
    tern.axis.title.show           = getOption("tern.title.show"),
    
    #Arrow
    tern.axis.arrow                = element_line(),
    tern.axis.arrow.T              = element_line(),
    tern.axis.arrow.L              = element_line(),
    tern.axis.arrow.R              = element_line(),
    tern.axis.arrow.text           = element_text(),
    tern.axis.arrow.text.T         = element_text(),
    tern.axis.arrow.text.L         = element_text(),
    tern.axis.arrow.text.R         = element_text(),
    tern.axis.arrow.show           = getOption("tern.arrow.show"),
    tern.axis.arrow.sep            = getOption("tern.arrow.sep"),
    tern.axis.arrow.start          = getOption("tern.arrow.start"),
    tern.axis.arrow.finish         = getOption("tern.arrow.finish"),
    
    #Ticks
    tern.axis.ticks                = element_line(),
    tern.axis.ticks.major          = element_line(),
    tern.axis.ticks.major.T        = element_line(),
    tern.axis.ticks.major.L        = element_line(),
    tern.axis.ticks.major.R        = element_line(),
    tern.axis.ticks.length.major   = unit(10, 'pt'),  #1.0*base$axis.ticks.length,
    tern.axis.ticks.length.minor   = unit( 5, 'pt'),  #0.5*base$axis.ticks.length
    tern.axis.ticks.outside        = getOption("tern.ticks.outside"),
    tern.axis.ticks.primary.show   = getOption("tern.ticks.primary.show"),
    tern.axis.ticks.secondary.show = getOption("tern.ticks.secondary.show"),
    tern.axis.ticks.minor          = element_line(),
    tern.axis.ticks.minor.T        = element_line(),
    tern.axis.ticks.minor.L        = element_line(),
    tern.axis.ticks.minor.R        = element_line(),
    
    #Panel Grids
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
    tern.panel.rotate              = getOption('tern.rotate'),
    
    ## DO NOT EDIT BELOW
    element_tree = list(
      tern.panel.background          = el_def("element_rect", "panel.background",     description="Background of Ternary Plot Area**"),
      tern.plot.background           = el_def("element_rect", "plot.background",      description="Background of Ternary Clipping Area**"),
      tern.plot.latex                = el_def("logical",                              description="Whether to parse characters as latex commands"),
      tern.axis.hshift               = el_def("numeric",                              description="Amount to nudge the plot horizontally"),
      tern.axis.vshift               = el_def("numeric",                              description="Amount to nudge the plot vertically"),
      tern.axis.clockwise            = el_def("logical",                              description="Clockwise or Anticlockwise Precession"),
      tern.axis.line                 = el_def("element_line", "axis.line",            description="Base Line"),
      tern.axis.line.T               = el_def("element_line", "tern.axis.line",       description="Line for TOP Axis"),
      tern.axis.line.L               = el_def("element_line", "tern.axis.line",       description="Line for LHS Axis"),
      tern.axis.line.R               = el_def("element_line", "tern.axis.line",       description="Line for RHS Axis"),
      tern.axis.line.ontop           = el_def("logical",                              description="Bring Axis Borders on Top of Everything (Depreciated)"),
      tern.axis.text                 = el_def("element_text", "axis.text",            description="Base Text"),
      tern.axis.text.T               = el_def("element_text", "tern.axis.text",       description="Text for TOP Axis"),
      tern.axis.text.L               = el_def("element_text", "tern.axis.text",       description="Text for LHS Axis"),
      tern.axis.text.R               = el_def("element_text", "tern.axis.text",       description="Text for RHS Axis"),
      tern.axis.text.show            = el_def("logical",                              description="Axis Labels Show or Hide"),
      tern.axis.title                = el_def("element_text", "axis.title",           description="Base Apex Title"),
      tern.axis.title.T              = el_def("element_text", "tern.axis.title",      description="Apex Title for TOP Axis"),
      tern.axis.title.L              = el_def("element_text", "tern.axis.title",      description="Apex Title for LHS Axis"),
      tern.axis.title.R              = el_def("element_text", "tern.axis.title",      description="Apex Title for RHS Axis"),
      tern.axis.title.show           = el_def("logical",                              description="Apex Titles Show or Hide"),
      tern.axis.arrow                = el_def("element_line", "axis.line",            description="Base Arrow Line"),
      tern.axis.arrow.T              = el_def("element_line", "tern.axis.arrow",      description="Arrow Line for TOP Axis"),
      tern.axis.arrow.L              = el_def("element_line", "tern.axis.arrow",      description="Arrow Line for LHS Axis"),
      tern.axis.arrow.R              = el_def("element_line", "tern.axis.arrow",      description="Arrow Line for RHS Axis"),
      tern.axis.arrow.text           = el_def("element_text", "tern.axis.text",       description="Base Arrow Label"),
      tern.axis.arrow.text.T         = el_def("element_text", "tern.axis.arrow.text", description="Arrow Label on TOP Axis"),
      tern.axis.arrow.text.L         = el_def("element_text", "tern.axis.arrow.text", description="Arrow Label on LHS Axis"),
      tern.axis.arrow.text.R         = el_def("element_text", "tern.axis.arrow.text", description="Arrow Label on RHS Axis"),
      tern.axis.arrow.show           = el_def("logical",                              description="Arrows Show or Hide"),
      tern.axis.arrow.sep            = el_def("numeric",                              description="Arrows Seperation from Axis"),
      tern.axis.arrow.start          = el_def("numeric",                              description="Proportion of Axis when Arrow Starts"),
      tern.axis.arrow.finish         = el_def("numeric",                              description="Proportion of Axis when Arrow Finishes"),
      tern.axis.ticks                = el_def("element_line", "axis.ticks",           description="Base Ticks"),
      tern.axis.ticks.major          = el_def("element_line", "tern.axis.ticks",      description="Base Major Ticks"),
      tern.axis.ticks.major.T        = el_def("element_line", "tern.axis.ticks.major",description="Base Major Ticks for TOP Axis"),
      tern.axis.ticks.major.L        = el_def("element_line", "tern.axis.ticks.major",description="Base Major Ticks for LHS Axis"),
      tern.axis.ticks.major.R        = el_def("element_line", "tern.axis.ticks.major",description="Base Major Ticks for RHS Axis"),
      tern.axis.ticks.length.major   = el_def("unit",                                 description="Ticks Major Ticklength"),
      tern.axis.ticks.length.minor   = el_def("unit",                                 description="Ticks Minor Ticklength"),
      tern.axis.ticks.outside        = el_def("logical",                              description="Ticks Outside or Inside"),
      tern.axis.ticks.primary.show   = el_def("logical",                              description="Ticks Show Primary"),
      tern.axis.ticks.secondary.show = el_def("logical",                              description="Ticks Show Secondary"),
      tern.axis.ticks.minor          = el_def("element_line", "tern.axis.ticks",      description="Base Minor Ticks"),
      tern.axis.ticks.minor.T        = el_def("element_line", "tern.axis.ticks.minor",description="Base Minor Ticks for TOP Axis"),
      tern.axis.ticks.minor.L        = el_def("element_line", "tern.axis.ticks.minor",description="Base Minor Ticks for LHS Axis"),
      tern.axis.ticks.minor.R        = el_def("element_line", "tern.axis.ticks.minor",description="Base Minor Ticks for RHS Axis"),
      tern.panel.grid.major          = el_def("element_line", "panel.grid.major",     description="Base Major Gridline"),
      tern.panel.grid.major.T        = el_def("element_line", "tern.panel.grid.major",description="Major Gridline for TOP Axis"),
      tern.panel.grid.major.L        = el_def("element_line", "tern.panel.grid.major",description="Major Gridline for LHS Axis"),
      tern.panel.grid.major.R        = el_def("element_line", "tern.panel.grid.major",description="Major Gridline for RHS Axis"),
      tern.panel.grid.major.show     = el_def("logical",                              description="Show or Hide Major Gridline"),
      tern.panel.grid.minor          = el_def("element_line", "panel.grid.minor",     description="Base Minor Gridline"),
      tern.panel.grid.minor.T        = el_def("element_line", "tern.panel.grid.minor",description="Minor Gridline for TOP Axis"),
      tern.panel.grid.minor.L        = el_def("element_line", "tern.panel.grid.minor",description="Minor Gridline for LHS Axis"),
      tern.panel.grid.minor.R        = el_def("element_line", "tern.panel.grid.minor",description="Minor Gridline for RHS Axis"),
      tern.panel.grid.minor.show     = el_def("logical",                              description="Show or Hide Minor Gridline"),
      tern.panel.grid.ontop          = el_def("logical",                              description="Bring grids, axis and axis labels on top of everything else"),
      tern.panel.mask.show           = el_def("logical",                              description="Show or Hide the Clipping Mask"),
      tern.panel.expand              = el_def("numeric",                              description="The amount to expand the ternary plotting panel, in ratio to npc units"),
      tern.panel.rotate              = el_def("numeric",                              description="The amount to rotate the ternary diagram in degrees")
    )
  )
}
