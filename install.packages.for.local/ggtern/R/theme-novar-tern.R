# ################################################################
# ### start code
#' @title
#' Blank one variable's annotations in ternary plot
#'
#' @description
#' This function blanks the grid and axis elements for one variable in a ternary plot.
#'
#' @details
#' This function takes a user-specified character corresponding to one of the three
#' ternary variables, and constructs a \code{theme} function which adds blank elements
#' for that variable's grid elements and axis elements chosen from the \pkg{ggtern}
#' package.  This new function is then executed which "adds" this theme to the open
#' ternary plot.
#'
#' The logic of the species selection is pretty transparent so it may be possible to
#' customize this function to add further affected elements as desired.  However the
#' computing on the language which drives this function has not been thoroughly
#' tested.  Neither has this function been tested with non-ternary plots available in
#' the \pkg{ggplot2} framework.
#'
#' @param species
#' A character giving the species. Choices are "T", "L" and "R", but is not case sensitive
#'
#' @param \dots
#' Further arguments, including additional selections otherwise used in species
#'
#' @usage
#' theme_novar_tern(species, \dots)
#'
#' @return
#' This function is called for the side effect of adding a theme which actually blanks
#' the grid and axis elements for the chosen ternary species.
#'
#' @examples
#' base = ggtern() + theme_rgbg()
#' base + theme_novar_tern("L")
#' base + theme_novar_tern(c("T","L"))
#' base + theme_novar_tern('L',R)

#'
#' @author
#' Nicholas Hamilton, John Szumiloski
#' @rdname theme_novar_tern
#' @export
theme_novar_tern <- function(species, ...) {
  
  species = unique(c(as.character(species),sapply(substitute(list(...)), deparse)[-1]))
  if(length(species) > 1)
    return(lapply(species,ggtern::theme_novar_tern))
  
  species <- substring(toupper(species[1]),1,1)
  
  if(!{species %in% c("T","L","R")}) {
    stop("\nspecies must be one of `T`, `L`, or `R`.\n")
  }
  
  outfxn <- function() theme(    
    tern.axis.title       = element_blank(),
    tern.axis.line        = element_blank(),
    tern.axis.text        = element_blank(),
    tern.axis.ticks.major = element_blank(),
    tern.axis.ticks.minor = element_blank(),
    tern.panel.grid.major = element_blank(),
    tern.panel.grid.minor = element_blank(),
    tern.axis.arrow       = element_blank(),  
    tern.axis.arrow.text  = element_blank()
  )
  
  names(body(outfxn))[-1] <- paste(names(body(outfxn))[-1], species, sep = ".")
  
  outfxn()
  
}
# ### end code
