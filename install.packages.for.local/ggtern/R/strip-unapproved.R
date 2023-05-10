#' Approved Geoms, Stats and Positions
#' 
#' \code{ggtern} is a specialist extension to \code{\link[=ggplot]{ggplot2}} for rendering ternary diagrams, as such, many stats and 
#' geoms which come packaged with \code{\link[=ggplot]{ggplot2}} are either not relevant or will not work, as such, 
#' \code{ggtern} regulates during the plot construction process, which geoms and stats are able to be applied 
#' when using the \code{\link{coord_tern}} coordinate system. Attempting to apply non-approved geometries or stats 
#' (ie geometries / stats not in the below list), will result in the respective layers being stripped from the final plot.
#' 
#' @section Approved Geometries:
#' \Sexpr[results=rd,stage=build]{ggtern:::.rd_approvedX('geom')}
#' 
#' @section Approved Stats:
#' \Sexpr[results=rd,stage=build]{ggtern:::.rd_approvedX('stat')}
#' 
#' @section Approved Positions:
#' \Sexpr[results=rd,stage=build]{ggtern:::.rd_approvedX('position')}
#' 
#' The balance of the available stats, geometries or positions within ggplot2 are either invalid or remain work in 
#' progress with regards to the \code{ggtern} package.
#' 
#' @aliases approved_stat approved_geom approved_position
#' @author Nicholas Hamilton
#' @name approved_layers
#' @rdname approved_layers
NULL

#' Strip Unapproved Layers
#' 
#' \code{strip_unapproved} is an internal function which essentially 'deletes' layers 
#' from the current ternary plot in the event that such layers are not one of the 
#' approved layers. For a layer to be approved, it must use an approved geometry, and also an approved stat. 
#' Refer to \link{approved_layers} for the current list of approved geometries and stats
#' 
#' @param layers list of the layers to strip unnaproved layers from.
#' @return \code{strip_unapproved} returns a list of approved layers (may be empty if none are approved).
strip_unapproved <- function(layers){
  layers = lapply(seq_along(layers),function(ix){
    layer    <- layers[[ix]]
    f <- function(type,name)
      sprintf("Removing Layer %i ('%s'), as it is not an approved %s (for ternary plots) under the present ggtern package.", 
              ix,paste(name,collapse="', '"),type)
    n = class(layer$geom)[1]
    if(!any(n %in% .approvedgeom)){
      warning(f('geometry',n),call.=FALSE)
      return(NULL)
    }
    n = class(layer$stat)[1]
    if(!any(n %in% .approvedstat)){
      warning(f('stat',n),call.=FALSE)
      return(NULL)
    }
    n = class(layer$position)[1]
    if(!any(n %in% .approvedposition)){
      warning(f('position',n),call.=FALSE)
      return(NULL)
    }
    layer
  })
  compact(layers)
}

#LIST OF APPROVED GEOMS
.approvedgeom      <- c(point            = "GeomPoint",
                        path             = "GeomPath",
                        line             = "GeomLine",
                        label            = "GeomLabel",
                        text             = "GeomText",
                        jitter           = "GeomPoint",
                        Tline            = "GeomTline",
                        Rline            = "GeomRline",
                        Lline            = "GeomLline",
                        polygon          = "GeomPolygon",
                        segment          = "GeomSegment",
                        count            = "GeomCount",
                        errorbarT        = "GeomErrorbart",
                        errorbarL        = "GeomErrorbarl",
                        errorbarR        = "GeomErrorbarr",
                        density_tern     = "GeomDensityTern",
                        confidence       = "GeomConfidenceTern",
                        curve            = "GeomCurve",
                        mask             = "GeomMask",
                        smooth_tern      = "GeomSmoothTern",
                        blank            = "GeomBlank",
                        jitter           = "GeomJitter",
                        Tisoprop         = "GeomTisoprop",
                        Lisoprop         = "GeomLisoprop",
                        Risoprop         = "GeomRisoprop",
                        interpolate_tern = "GeomInterpolateTern",
                        crosshair_tern   = "GeomCrosshairTern",
                        Tmark            = "GeomTmark",
                        Lmark            = "GeomLmark",
                        Rmark            = "GeomRmark",
                        point_swap       = "GeomPointSwap",
                        rect             = "GeomRect",
                        polygon_closed   = "GeomPolygonClosed",
                        hex_tern         = "GeomHexTern",
                        tri_tern         = "GeomTriTern",
                        "GeomRasterAnnTern",
                        "GeomDl",
                        "GeomEncircle",
                        "GeomCustomAnn",
                        mean_ellipse     = "GeomMeanEllipse",
                        text_viewport    = "GeomTextViewport",
                        label_viewport   = "GeomLabelViewport"
)

#LIST OF APPROVED STATS
.approvedstat     <- c( identity         = "StatIdentity",
                        confidence       = "StatConfidenceTern",
                        density_tern     = "StatDensityTern",
                        smooth_tern      = "StatSmoothTern",
                        sum              = "StatSum",
                        unique           = "StatUnique",
                        interpolate_tern = "StatInterpolateTern",
                        mean_ellipse     = "StatMeanEllipse",
                        hex_tern         = "StatHexTern",
                        tri_tern         = "StatTriTern"
)

#LIST OF APPROVED POSITIONS
.approvedposition <- c(identity          = "PositionIdentity",
                       nudge_tern        = "PositionNudgeTern",
                       jitter_tern       = "PositionJitterTern"
)

#Method for building rd file
.rd_approvedX <- function(type="geom"){
  if(!type %in% c('geom','stat','position')) 
    stop("Invalid type",call.=FALSE)
  
  theNames = .nonBlankNames(type)
  theObjs  = unlist(lapply(theNames,function(x){
    o = sprintf("%s_%s",type,x)
    #if(find(o) != 'package:ggplot2') 
    o = sprintf("\\link{%s}",o)
    o
  }))
  paste(sprintf("The following %ss have been approved so far, including a combination of existing %ss and newly created %ss for the ggtern package\n",type,type,type),
        sprintf("APPROVED %ss in \\code{ggtern} are as follows:\n\n\\itemize{\n",type),
        paste("\\item\\code{",theObjs,"}",collapse="\n",sep=""),
        "\n}\n",sep = "")
}

.nonBlankNames <- function(type){
  ret = names(get(sprintf(".approved%s",type)))
  ret[which(ret != '')]
}

