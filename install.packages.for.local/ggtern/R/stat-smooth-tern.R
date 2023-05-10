#' @rdname geom_smooth_tern
#' @export
stat_smooth_tern <- function( mapping = NULL, data = NULL,position = "identity", 
                              ...,
                              method = "auto",formula = y ~ x,
                              se = TRUE, n = 80, span = 0.75, fullrange = FALSE,
                              level = 0.95, method.args = list(),
                              na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,expand=c(0.5,0.5)) {
  layer(
    data        = data,
    mapping     = mapping,
    stat        = StatSmoothTern,
    geom        = 'smoothTern',
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      method      = method,
      formula     = formula,
      se          = se,
      n           = n,
      fullrange   = fullrange,
      level       = level,
      na.rm       = na.rm,
      method.args = method.args,
      span        = span,
      base        = 'identity',
      expand      = expand,
      ...
    )
  )
}

#' @rdname geom_smooth_tern
#' @format NULL
#' @usage NULL
#' @export
StatSmoothTern <- ggproto("StatSmoothTern", Stat,
  setup_params = function(data, params) {
    params = StatSmooth$setup_params(data,params)
    params
  },
  compute_group = function(self,data, scales, method = "auto", formula=y~x,
                           se = TRUE, n = 80, span = 0.75, fullrange = FALSE,
                           xseq = NULL, level = 0.95, method.args = list(),
                           na.rm = FALSE,expand=c(0.5,0.5)){
    base='identity' ##FIXED FOR MOMENT
    if (!base %in% c('identity','ilr')) stop('base must be either identity or ilr',call.=FALSE)
    if (is.character(method))  method  <- match.fun(method)
    if (is.character(formula)) formula <- as.formula(formula)
    if (length(expand) != 2)   expand  <- rep(expand[1],2); stopifnot(is.numeric(expand))
    
    #Variables
    coord       = coord_tern()
    raes        = self$required_aes
    
    #Check that the data is valid
    data[,raes] = data.frame(acomp(data[,raes]))
    data        = remove_missing(data,vars=raes,na.rm=TRUE,name=class(self)[1],finite=TRUE)
    if(empty(data))return(zeroGrob())
    
    #Transform
    if(base == 'ilr')
      data[,c('x','y')] = as.data.frame(ilr(data[,self$required_aes]))
    else
      data = tlr2xy(data,coord,inverse=FALSE,scale=TRUE)
    
    #Backup, This is a hack if data was not provided where it sums to 1, the ranges can be out.
    bupxlims = scales$x$limits; bupylims = scales$y$limits
    if(fullrange){
      scales$x$limits = expand_range(range(data$x),expand)
      scales$y$limits = expand_range(range(data$y),expand)
    }
    
    #Do the computation
    data = StatSmooth$compute_group(data,scales,method,formula,se,n,span,fullrange,xseq,level,method.args,na.rm)
    
    #Add necessary columns
    if(se){  data$xmin = data$x; data$xmax = data$x; }
    
    #Restore
    scales$x$limits = bupxlims; scales$y$limits = bupylims
    
    #Transform back to ternary space
    if(base == 'ilr'){
      for(grp in c('', (if(se) c('min','max') else NULL) )){
        cols.s = paste(self$required_aes[1:2],grp,sep="")
        cols.t = paste(self$required_aes,     grp,sep="")
        if(all(cols.s %in% names(data))) data[,cols.t] = as.data.frame(ilrInv(data[,cols.s]))
      }
    }else{
      if(se){
        ix        = c('ymin','ymax')
        data[,ix] = data.frame( apply(data[,ix],1,min),
                                apply(data[,ix],1,max))
      }
      data = tlr2xy(data,coord,inverse=TRUE,scale=TRUE)
    }
    data
  },
  required_aes = c("x", "y","z")
)