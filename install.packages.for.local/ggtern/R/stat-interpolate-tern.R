#' @inheritParams stat_density_tern
#' @rdname geom_interpolate_tern
#' @export
stat_interpolate_tern <- function(mapping = NULL, data = NULL, geom = "interpolate_tern",position = "identity",
                                  ...,
                                  method='auto', na.rm = FALSE, show.legend = NA,
                                  inherit.aes = TRUE, n=80, formula=value~poly(x,y,degree=1),base='ilr') {
  layer(
    data        = data,
    mapping     = mapping,
    stat        = StatInterpolateTern,
    geom        = geom,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(
      na.rm     = na.rm,
      formula   = formula,
      method    = method,
      n         = n,
      base      = base,
      ...
    )
  )
}

#' @rdname geom_interpolate_tern
#' @usage NULL
#' @format NULL
#' @export
StatInterpolateTern <- ggproto("StatInterpolateTern", 
  Stat,
  retransform   = FALSE,
  required_aes  = c("x", "y", "z", "value"),
  default_aes   = aes(order = ..level..),
  setup_params  = function(data, params) {
    if (identical(params$method, "auto")) {
      max_group <- max(table(data$group))
      
      if(max_group < 1000){
         params$method = 'loess'
      }else{
        params$method  = 'glm'
      }
    }
    
    params
  },
  compute_group = function(self, data, scales, method='auto', bins = NULL, binwidth = NULL, breaks = NULL, 
                           complete  = FALSE, na.rm = FALSE, formula=value~poly(x,y,degree=1),closure='none', 
                           fullrange = FALSE, n = 80, h = 6, expand=0.5, method.args=list(),base='ilr') {
    
    if(!base %in% c('identity','ilr')) 
      stop('base must be either identity or ilr',call.=FALSE)
    
    #Check required aesthetics
    ggint$check_required_aesthetics(self$required_aes, names(data), ggint$snake_class(self))
    
    #Ensure it is a composition
    data[self$required_aes[1:3]] = acomp(data[self$required_aes[1:3]])
    
    #Determine the Breaks
    values = data[self$required_aes[4]]
    if (is.null(bins) && is.null(binwidth) && is.null(breaks))
      breaks <- pretty(range(values), 10)
    if (!is.null(bins))
      binwidth <- diff(range(values))/bins
    if (is.null(breaks))
      breaks <- fullseq(range(values), binwidth)
    
    #Build the ternary grid
    #theGrid = .getGrid(data,n,fullrange,expand=0)
    
    #Transform the data into the orthonormal space
    din  = data[self$required_aes[1:3]]
    func = if(base == 'ilr') base else tlr2xy
    args = if(base == 'ilr') list(x=din) else list(data=din,coord=coord_tern(),inverse=FALSE)
    data[self$required_aes[1:2]] = do.call(func,args=args)
    
    #Build the model
    base.args = list(formula = quote(formula), 
                     data    = quote(data))
    model     = do.call(method, c(base.args, method.args))
    
    ##Check expand is vector of 2
    expand   = if(length(expand) != 2) rep(expand[1],2) else expand
    
    #New Data to Predict
    xrng    = expand_range(range(data[self$required_aes[1]]),expand[1])
    yrng    = expand_range(range(data[self$required_aes[2]]),expand[2])
    
    #Inverse-Log-Ratio Space
    if(base == 'ilr'){
      mirr = function(x) c(-rev(x),x) #Mirror Function
      n    = max( ceiling(n/2) ,1)    #Half n, since going to mirror
      xseq = mirr( exp(-seq(0,h,length.out = n)) * max(abs(xrng)) )
      yseq = mirr( exp(-seq(0,h,length.out = n)) * max(abs(yrng)) )
    
    #Cartesian and Other
    }else{
      xseq = seq(xrng[1],xrng[2],length.out=n)
      yseq = seq(yrng[1],yrng[2],length.out=n)
    }
    
    #Predict the data
    data = predictdf2d(model, xseq = xseq, yseq = yseq)
    data = data[which(complete.cases(data)),]

    
    #Draw the contours
    result    = StatContour$compute_group(data,scales,bins=bins,binwidth=binwidth,breaks=breaks,na.rm=na.rm)

      if(closure %in% c('upper','lower')){
        result = ddply(result,setdiff(names(result),c('x','y')),function(df){
          row      = df[nrow(df),,drop=F]
          ix       = c(1,nrow(df))
          if(closure == 'upper'){
            row$x = max(df$x)
            row$y = max(df$y)
            rbind(df,row)
          }else{
            row$x = min(df$x)
            row$y = min(df$y)
            rbind(row,df)
          }
        })
      }
 
    #Transform the data out of orthonormal space, into ternary space
    din  = result[self$required_aes[1:2]]
    func = if(base == 'ilr') sprintf("%sInv",base) else tlr2xy
    args = if(base == 'ilr') list(z=din) else list(data = din, coord  = coord_tern(), inverse=TRUE)
    result[self$required_aes[1:3]] = do.call(func,args)
    
    #Done
    result
  }
)


