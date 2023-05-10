#'Internal Functions
#'
#'@description INTERNAL FUNCTIONS: \code{ggtern} makes use of several non-exported internal functions, list are as follows:
#'@keywords internal
#'@rdname undocumented
#'@name zzz-internal
NULL

#' \code{ifthenelse} function takes input arguments \code{x}, \code{a} and \code{b} and returns \code{a} if \code{x} is \code{TRUE}, else, returns \code{b}
#' @param x logical input to check
#' @param a value to return if \code{x} is TRUE
#' @param b value to return if \code{x} is FALSE
#' @keywords internal
#' @rdname undocumented
ifthenelse <- function(x,a,b){
  if(!is.logical(x))stop("x argument must be logical")
  if(x){a}else{b}
}

#' \code{is.numericor} function takes input arguments \code{A} and \code{B} and returns \code{A} if \code{A} is numeric, else, returns \code{B}
#' @param A value to return if numeric
#' @param B numeric value to return if \code{A} is NOT numeric
#' @keywords internal
#' @rdname undocumented
is.numericor <- function(A,B){
  if(!is.numeric(B)){stop("b must be numeric")}
  if(is.numeric(A)){A}else{B}
}

"%||%"  <- function(a, b) {if (!is.null(a)) a else b}

#' \code{find_global_tern} is a function that conducts a named search for the \code{name} object instance, within the \code{env} environment. 
#' If an instance doesn't exist within the \code{env} environment, a search is then conducted within the \code{ggtern} and \code{ggplot2} 
#' namespaces \emph{(in that order)}. This is a modified version of the original source as provided in \code{ggplot2}, which has the same functionality, however, the modification is such that the function
#' now additionally searches within the \code{ggtern} namespace prior to the \code{ggplot2} namespace.
#' @param name character name of object to search for
#' @param env environment to search within as first priority
#' @param mode the mode to search within
#' @keywords internal
#' @rdname undocumented
#' @export
find_global_tern <- function (name, env=environment(),mode='any'){  
  if(!is.character(name)){stop("'name' must be provided as a character")}
  if(!inherits(environment(),"environment")){stop("'env' must inherit the environment class")}
  
  if (exists(name, envir = env, mode = mode)){ 
    return(get(name, envir = env, mode = mode))
  }
  
  nsenv <- asNamespace("ggtern")
  if(exists(name, envir = nsenv, mode=mode)){
    return(get(name, envir = nsenv, mode = mode))
  }
  
  nsenv <- asNamespace("ggplot2")
  if(exists(name, envir = nsenv, mode=mode)){
    return(get(name, envir = nsenv, mode = mode))
  }
  
  NULL
}

#' Convert RGB to HEX Color
#' 
#' Function to convert rgb color to hex color
#' @param r,g,b colors, numeric scalar between 0 and 255
#' @keywords internal
#' @author Nicholas Hamilton
#' @examples 
#' #Black
#' rgb2hex(0,0,0)
#' 
#' #White
#' rgb2hex(255,255,255)
#' 
#' #Red
#' rgb2hex(255,0,0)
#' 
#' #Green
#' rgb2hex(0,255,0) 
#' 
#' #Blue
#' rgb2hex(0,0,255)
#' 
#' #Vectorised sequence of blue
#' rgb2hex(0,0,seq(0,255,by=5))
#' @export
rgb2hex = function(r = 0, g = 0, b = 0){
  df = data.frame(r, g, b)
  check = function(x, ix = NULL){
    nm = deparse(substitute(x))
    ix = as.character({ix %||% ''})
    if(!is.numeric(x))  stop(sprintf("'%s%s' must be numeric",             nm,ix),call. = FALSE)
    if(length(x) != 1)  stop(sprintf("'%s%s' must be scalar",              nm,ix),call. = FALSE)
    if(!is.finite(x))   stop(sprintf("'%s%s' must be finite",              nm,ix),call. = FALSE)
    if(x < 0 | x > 255) stop(sprintf("'%s%s' must be in the range [0,255]",nm,ix),call. = FALSE)
  }
  nr = nrow(df)
  sapply( c(1:nr), function(ix){
    n = if(nr > 1){ ix }else{ NULL }
    r = df$r[ix]; check(r,n)
    g = df$g[ix]; check(g,n)
    b = df$b[ix]; check(b,n)
    sprintf("#%.2x%.2x%.2x",r,g,b) 
  })
}

#' Generate Axis Breaks
#' 
#' Calculates the Breaks for Major or Minor Gridlines based on the input limits.
#' @param limits the scale limits
#' @param isMajor major or minor grids
#' @param n number of breaks
#' @rdname breaks_tern
#' @examples 
#'  breaks_tern()
#'  breaks_tern(limits = c(0,.5),FALSE,10)
#' @export
breaks_tern <- function(limits = c(0,1), isMajor = TRUE, n = 5){
  if(is.null(limits) || !all(is.numeric(limits)))
    limits = c(0,1)
  
  if(diff(range(limits)) == 0){
    ret = if(isMajor) getOption("tern.breaks.default") else getOption("tern.breaks.default.minor")
    return(ret)
  }
    
  ret = pretty(limits,n = n)
  if(!isMajor){
    r = range(ret)
    d = diff(r)/(length(ret)-1)
    minor = seq(min(ret)-d/2,max(ret)+d/2,by = d)
    minor = minor[which(minor > min(limits) & minor < max(limits))]
    ret   = minor[which(!minor %in% ret)]
  }
  ret
}


#' @rdname breaks_tern
#' @name breaks_tern
#' @usage NULL
#' @format NULL
#' @export
getBreaks = function(limits = c(0,1), isMajor = TRUE, n = 5){
  tern_dep("2.1.4","'getBreaks' has been superceded by the 'breaks' function")
  breaks_tern(limits,isMajor,n)
}

#' Generate Axis Labels
#' 
#' Calculates the Labels for Major or Minor Gridlines based on the input limits.
#' @param breaks numeric denoting the breaks to produce corresponding labels
#' @inheritParams breaks_tern
#' @param format the formatting string to be passed through to the \code{\link{sprintf}} function
#' @param factor the multiplicative factor
#' @examples 
#' labels_tern()
#' labels_tern(limits = c(0,.5))
#' @author Nicholas Hamilton
#' @rdname labels_tern
#' @export
labels_tern = function(limits = c(0,1), breaks = breaks_tern(limits), format = "%g", factor = 100){
  if(!is.numeric(breaks)) 
    stop("'breaks' must be numeric",call.=FALSE)
  
  #Default Result
  result = factor[1]*breaks
  
  #Try and process...
  tryCatch({
    if(!is.numeric(factor)) 
      stop("'factor' must be numeric",call.=FALSE)
    result = sprintf(format,factor[1]*breaks)
    
    #Stop First Label interfering with the main label
    if(breaks[1] == min(limits))
      result[1] = ''
    
  },error=function(e){ })
  
  #Done
  result
}

#' @rdname labels_tern
#' @name labels_tern
#' @usage NULL
#' @format NULL
#' @export
getLabels = function(limits = c(0,1), breaks = breaks_tern(limits), format = "%g", factor = 100){
  tern_dep("2.1.4","'getBreaks' has been superceded by the 'breaks' function")
  labels_tern(limits,breaks,format,factor)
}

#' \code{tern_dep} is a function that gives a deprecation error, warning, or messsage, 
#' depending on version number, it is based of the \code{\link[ggplot2]{gg_dep}} function which is
#' used inside the \code{ggplot2} package
#' @inheritParams ggplot2::gg_dep
#' @keywords internal
#' @rdname undocumented
tern_dep <- function(version, msg) {
  v <- as.package_version(version)
  cv <- packageVersion("ggtern")
  
  # If current major number is greater than last-good major number, or if
  #  current minor number is more than 1 greater than last-good minor number,
  #  give error.
  if (cv[[1,1]] > v[[1,1]]  ||  cv[[1,2]] > v[[1,2]] + 1) {
    stop(msg, " (Defunct; last used in version ", version, ")",
         call. = FALSE)
    
    # If minor number differs by one, give warning
  } else if (cv[[1,2]] > v[[1,2]]) {
    warning(msg, " (Deprecated; last used in version ", version, ")",
            call. = FALSE)
    
    # If only subminor number is greater, give message
  } else if (cv[[1,3]] > v[[1,3]]) {
    message(msg, " (Deprecated; last used in version ", version, ")")
  }
  
  invisible()
}

#internal
.makeValid <- function(x){
  x = x[[1]]
  if(is(x,'character')){
    x = gsub("%","'%'",x)
    x = gsub('([[:punct:]])\\1+', '\\1', x)
    x = gsub(" ","~",x)
  }
  x
}

#' \code{arrow_label_formatter} is a function that formats the labels directly adjacent to the ternary arrows.
#' @param label character label
#' @param suffix chacater suffix behind each label
#' @param sep the seperator between label and suffix 
#' @param ... additional arguments
#' @param latex logical as to whether latex formats should be parsed
#' @keywords internal
#' @rdname undocumented
arrow_label_formatter             = function(label,suffix=NULL,sep="/",...) UseMethod("arrow_label_formatter")
arrow_label_formatter.default     = function(label,suffix=NULL,sep="/",...) arrow_label_formatter.character( as.character(label), suffix, sep, ...)
arrow_label_formatter.call        = function(label,suffix=NULL,sep="/",...) arrow_label_formatter.expression(as.expression(label),suffix, sep, ...)    
arrow_label_formatter.expression  = function(label,suffix=NULL,sep="/",...){
  suffix = if(suffix  == "")   NULL else suffix
  sep    = if(is.null(suffix)) ""   else .trimAndPad(sep)
  parse(text=paste(as.character(label),suffix,sep))
}
arrow_label_formatter.character   = function(label,suffix=NULL,sep="/",latex = FALSE,...) {
  suffix = if(suffix  == "")   NULL else suffix
  sep    = if(is.null(suffix)) ""   else .trimAndPad(sep)
  result = paste(label,suffix,sep=sep)
  if(latex[1]) result = TeX(result)
  result
}
.trimAndPad <- function(x){
  x = gsub("^(\\s+)","",gsub("(\\s+)$","",x))
  if(nchar(x) == 1) x = sprintf(" %s ",x)
  x
}


#' \code{label_formatter} is a function that formats / parses labels for use in the grid.
#' @param label character label
#' @param ... additional arguments
label_formatter = function(label,...){ arrow_label_formatter(label,suffix="",sep="",...) }


#' \code{joinCharacterSeries} is a function will turn a character vector 
#' from the format \code{c('a','b','c')} to a single string
#' in the following format: \code{"'a','b' and 'c'"}
#' @param x character vector
#' @author Nicholas Hamilton
#' @keywords internal
#' @rdname undocumented
joinCharacterSeries <- function(x,lastWord='and'){
  if(!is.character(x) | !is.vector(x)) stop("'x' must be character vector",call.=FALSE)
  if(length(x) > 1){ x = paste(paste(x[-length(x)],collapse="', '"),x[length(x)],sep=sprintf("' %s '",lastWord)) }
  sprintf("'%s'",x)
}


#' \code{identityInv} is a function which returns exactly the same as \code{\link{identity}} however
#' it can be used within transformation logic via \code{do.call(...)} in the same way as for example
#' \code{\link{ilrInv}} is to \code{\link{ilr}}.
#' @param x input object
#' @author Nicholas Hamilton
#' @keywords internal
#' @rdname undocumented
identityInv = function(z) identity(z)


#' \code{getFormulaVars} is a function that returns a list of either dependent or independent variables used
#' in an input formula
#' @param x formula object
#' @param dependent whether to return the dependent variables (TRUE) or the indpenedent variables (FALSE)
#' @rdname undocumented
#' @keywords internal
#' @author Nicholas Hamilton
getFormulaVars = function(x,dependent=TRUE) {
  if(!is(x,'formula')) stop("x argument must be a formula",call.=FALSE)
  all.vars(x[[if(dependent) 3 else 2]])
}

#' Function to add missing scales and other items to the plot and its coordinates sytem
#' @param ggplot object
#' @rdname undocumented
#' @keywords internal
#' @author Nicholas Hamilton
scales_add_missing_tern <- function(plot){
  
  #Run some checks
  stopifnot(inherits(plot,'ggplot'))
  stopifnot(inherits(plot$coordinates,'CoordTern'))
  
  #Ensure required scales have been added
  rs = plot$coordinates$required_scales
  
  aesthetics  = setdiff(rs, plot$scales$input())
  env = plot$plot_env
  for (aes in aesthetics) {
    scale_name <- paste("scale", aes, "continuous", sep = "_")
    scale_f <- find_global_tern(scale_name, env, mode = "function")
    plot$scales$add(scale_f())
  }
  
  #ggint$scales_add_missing(plot,rs,plot$plot_env) ##NH
  #plot$scales$scales = plot$scales$scales[!sapply(plot$scales$scales,is.null)] 
  #plot$scales$scales = compact(plot$scales$scales)
  
  #Push some details to the coordinates
  plot$coordinates$scales        = sapply(rs,plot$scales$get_scales) ##NH
  for(r in rs) 
    plot$coordinates$limits[[r]] = plot$scales$get_scales(r)$limits
  plot$coordinates$labels_coord  = plot$labels
  plot$coordinates$theme         = ggint$plot_theme(plot) #NH

  #done
  plot
}

#' Function to add clipping mask if it isn't already present
#' @param plot ggplot object
#' @rdname undocumented
#' @keywords internal
#' @author Nicholas Hamilton
layers_add_or_remove_mask = function(plot){
  theme = ggint$plot_theme(plot) #NH
  mask  = calc_element('tern.panel.mask.show',theme)[1] %||% TRUE
  if(is.na(mask) || mask){
    if(!"GeomMask" %in% unlist(lapply(plot$layers,function(x){ class(x$geom) })))
      plot = plot + geom_mask()
  }else{
    plot$layers = plyr::compact(lapply(plot$layers,function(x){
      if(inherits(x$geom,'GeomMask')) return(NULL) else x
    }))
  }
  plot
} 



