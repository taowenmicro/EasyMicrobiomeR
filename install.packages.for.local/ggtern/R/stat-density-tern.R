#' @inheritParams ggplot2::stat_density2d
#' @inheritParams geom_density_tern
#' @param base the base transformation of the data, options include 'identity' (ie direct on the cartesian space), or 'ilr'
#' which means to use the isometric log ratio transformation.
#' @param geom Use to override the default connection between geom_density_tern() and stat_density_tern()
#' @param h Bandwidth (vector of length two) as a multiple of the best estimate, estimated using \code{\link[MASS]{bandwidth.nrd}}. 
#' @param bdl the threshold for detection limit. This is applied against the output of \code{\link[compositions]{acomp}} function, 
#' so it is expected as a fraction in the range [0,1]
#' @param bdl.val compositions which have components that are below the detection limit, will have these components replaced by this val. 
#' If it is NA then these items will be discarded. If the value is something other than 'NA', then all values less than \code{bdl} will
#' be replaced and therefore included in the final density estimate.
#' @param weight weighting for weighted kde2d esimate, default's to 1, which is non-weighted and equivalent to the usual kde2d calculation
#' @param expand Calculate on a mesh which extends beyond the grid of the plot region by this amount
#' If \code{NULL}, estimated using \code{\link[MASS]{bandwidth.nrd}}.
#' @examples 
#' #Plot Density Estimate w/ Polygon Geometry
#' data('Feldspar')
#' ggtern(data=Feldspar,aes(Ab,An,Or)) + 
#'     stat_density_tern(
#'         geom='polygon',
#'         aes(fill=..level..),
#'         bins=5,
#'         color='grey') +
#'     geom_point()
#'         
#' @author Nicholas Hamilton
#' @rdname geom_density_tern
#' @export
stat_density_tern <- function(mapping = NULL, data = NULL, geom = "density_tern",position = "identity",
                              ...,
                              contour = TRUE,
                              n = 100, h = NULL, bdl = 0, bdl.val = NA, na.rm = FALSE,
                              show.legend = NA, inherit.aes = TRUE,weight=1,base='ilr',expand = c(.5,.5)) {
  layer(
    data        = data,
    mapping     = mapping,
    stat        = StatDensityTern,
    geom        = geom,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(
      na.rm     = na.rm,
      contour   = contour,
      n         = n,
      h         = h,
      bdl       = bdl,
      bdl.val   = bdl.val,
      base      = base,
      expand    = expand,
      weight    = weight,
      ...
    ) 
  )  
}

#' @rdname geom_density_tern
#' @format NULL
#' @usage NULL
#' @export
StatDensityTern  = ggproto("StatDensityTern", 
  Stat,
  retransform  = FALSE,
  required_aes = c("x",'y',"z"),
  setup_data = function(data, params) {
    data$weight = data$weight %||% params$weight %||% 1
    data
  },
  compute_group  = function(self,data, scales, 
                            na.rm    = FALSE, 
                            n        = 100, 
                            h        = NULL,
                            bdl      = 0,
                            bdl.val  = NA,
                            contour  = TRUE,
                            base     = 'ilr', 
                            expand   = 0.5, 
                            weight   = NULL, 
                            bins     = NULL, 
                            binwidth = NULL,
                            breaks   = NULL) {
    
    if(!c(base) %in% c('identity','ilr')) 
      stop('base must be either identity or ilr',call.=FALSE)
    
    #Required aesthetics
    raes  = self$required_aes
    
    ##Set values below the detection limit to the below detection limit value
    data[raes] = suppressWarnings(compositions::acomp(data[raes]))
    data[raes][data[raes] <= bdl] = bdl.val[1]
    
    ##Remove missing values
    data   = remove_missing(data,vars=self$required_aes,na.rm=na.rm,name='StatDensityTern',finite=TRUE)
    if(empty(data)) 
      return(data.frame())
    
    if(base == 'ilr' && bdl <= 0){
      MINIMUM_BDL = 0.01
      x  = data[raes]
      ix = which(apply(x,1,min) < MINIMUM_BDL)
      if(length(ix) > 0){
        nBDL = length(ix)
        msg = 
"%s: You have not specified a below-detection-limit (bdl) value (Ref. 'bdl' and 'bdl.val' arguments in ?stat_density_tern). Presently you have %sx value/s below a detection limit of %.3f, which acounts for %.3f%% of your data. Density values at fringes may appear abnormally high attributed to the mathematics of the ILR transformation. 
You can either:
1. Ignore this warning,
2. Set the bdl value appropriately so that fringe values are omitted from the ILR calculation, or
3. Accept the high density values if they exist, and manually set the 'breaks' argument 
   so that the countours at lower densities are represented appropriately."
        msg = sprintf(msg,ggint$snake_class(self),nBDL,MINIMUM_BDL,100*nBDL/nrow(data))
        warning(msg,call.=FALSE)
      }
    }
    
    ##Default Coordinates
    coord = coord_tern()
    
    f     = get(base,mode='function')
    fInv  = get(sprintf("%sInv",base),mode='function')

    #To cartesian coordinates
    if(base == 'identity') 
      data = tlr2xy(data,coord,inverse=FALSE,scale=TRUE)
    
    #Now resolve the bandwidth
    h = h %||% estimateBandwidth(base,data[which(colnames(data) %in% raes)]); if(length(h) != 2) h = rep(h[1],2)
    if(base != 'identity' && diff(h) != 0) ##for ilr, h[1] should be the same as h[2]
      warning("bandwidth 'h' has different x and y bandwiths for 'ilr', this may (probably will) introduce permutational artifacts depending on the ordering",call.=FALSE)
    
    ##NH: Bug in Compositions package, using suppress warnings
    data[raes[1:2]] = suppressWarnings(f(as.matrix(data[which(colnames(data) %in% raes)])))
    
    expand   = `if`(length(expand) != 2, rep(expand[1],2) , expand )
    rngxy    = range(c(data$x,data$y))
    rngx     = expand_range(switch(base,'identity'= coord$limits$x,rngxy ),expand[1]) ##If not identity, x and y range should be same
    rngy     = expand_range(switch(base,'identity'= coord$limits$y,rngxy ),expand[2])
    
    #Do the density estimate
    dens     = kde2d.weighted(data$x, data$y, h = h, n = n, lims = c(rngx,rngy), w = data$weight)
    
    #Put back into dataframe
    df       = data.frame(expand.grid(x = dens$x, y = dens$y), z = as.vector(dens$z),group=data$group[1])
    
    # Divide by jacobian determinant.
    # Thanks to alexi19811
    if(base == 'ilr'){
      comps = fInv((df[,c('x','y')]))
      df$z  = df$z / (sqrt(3) * apply(comps,1,prod))
    }
    
    ##Build the contours
    if (contour) {
      z.range <- range(df$z, na.rm = TRUE, finite = TRUE)
      df = StatContour$compute_panel(df,scales,bins=bins,binwidth=binwidth,breaks=breaks,z.range=z.range)
    } else {
      names(df) <- c("x", "y", "density", "group")
      df$level <- 1
      df$piece <- 1
    }
    
    #Back to ternary coordinates
    if(base == 'identity') 
      df = tlr2xy(df,coord,inverse=TRUE,scale=TRUE)
    
    #Inverse transformation
    df[raes] = suppressWarnings(fInv(as.matrix(df[which(colnames(df) %in% raes)]) )) ##NH: Bug in compositions package, suppress warnings.
    
    #Done
    df
  }
)


estimateBandwidth = function(base='identity',df){
  
  ##Identity case
  if(base == 'identity'){
    mu = mean(MASS::bandwidth.nrd(df$x),MASS::bandwidth.nrd(df$y))
  
  ##ILR Case  
  }else{
    n     = 1:3
    eg    = expand.grid(x=n,y=n,z=n)
    eg    = eg[apply(eg,1,anyDuplicated) == 0, ]
    perms = t(apply(eg,1,function(r){
      df = suppressWarnings(compositions::ilr(df[,r]))
      c(MASS::bandwidth.nrd(df[,1]),MASS::bandwidth.nrd(df[,2]))
    }))
    mu = mean(perms)
  }
  return(rep(mu,2))
}

