#' Ternary / Cartesian Transformation
#' 
#' Functions to transform data from the ternary to cartesian spaces and vice-versa. 
#' 
#' @param data \code{data.frame} containing columns as required by the coordinate system. 
#' Data will be scaled so that the rows sum to unity, in the event that the user has provided 
#' data that does not.
#' @param coord Coordinate system object, inheriting the \code{\link{CoordTern}} class, error will
#' be thrown if a different coordinate system is sent to this method
#' @param ... not used
#' @param inverse logical if we are doing a forward (FALSE) or reverse (TRUE) transformation
#' @param scale logical as to whether the transformed coordinates are scaled (or reverse scaled in the case of inverse 
#' transformation) according to the training routine defined in the coordinate system.
#' @param drop drop all non columns which are not involved in the transformation
#' @author Nicholas Hamilton
#' @examples 
#' data(Feldspar)
#' dfm = plyr::rename(Feldspar,c("Ab"="x","An"="y","Or"="z"))
#' crd = coord_tern()
#' fwd = tlr2xy(dfm,crd)
#' rev = tlr2xy(fwd,crd,inverse = TRUE)
#' @rdname ternary_transformation
#' @name ternary_transformation
NULL

#' @details \code{tlr2xy} transforms from the ternary to cartesian spaces, an inverse transformation 
#' transforms between cartesian to ternary spaces
#' @rdname ternary_transformation
#' @export
tlr2xy <- function(data,coord,...,inverse=FALSE,scale=TRUE,drop=FALSE){
  
  #Run Some Checks
  if(!inherits(coord,"CoordTern")) 
    stop("argument 'coord' must be a CoordTern coordinate structure")
  if(!is(data,"data.frame"))
    stop("argument 'data' must be of type 'data.frame'")
  if(!is.logical(inverse) | !is.logical(scale))
    stop("argument 'inverse' and 'scale' (both) must be logical")
  
  #Determine the Proposed Mapping and Reverse Mapping
  mapping = coord$mapping; ix.trl  = as.character(coord$mapping); ix.xy   = c("x","y")
  if(length(unique(ix.trl)) != 3 | length(unique(names(mapping))) != 3) stop('Mapping must have 3 unique named variable pairs',call.=FALSE)
  mappingRev = mapping; for( key in names(mappingRev) ) mappingRev[key] <- key; names(mappingRev) = as.character(mapping)
  
  #Determine the required and the destination position aesthetics
  ix.req    = if(!inverse){ ix.trl }else{ ix.xy  }
  ix.dst    = if(!inverse){ ix.xy  }else{ ix.trl }
  
  #Determine the aesthetic set groups
  setGroups = .get.sets(ix.req,names(data))
  
  #If there are more than one set group, process recursively, and re-combine
  if(length(setGroups) > 1){
    #Get the full set of combinations
    ix.full      = .combos(ix.req,setGroups)
    #Check the missing aesthetics
    .check.aes(coord,ix.full,names(data),inverse=inverse)
    #Split the data into transformation and non-transformation data
    data.notrans = data[,which(!names(data) %in% ix.full),drop=FALSE]
    data.totrans = data[,which( names(data) %in% ix.full),drop=FALSE]
    #Now for each group, conduct a transformation, when re-transformed, combine and return
    #Transformation is called recursively
    for(group in setGroups){
      ix  = .combos(ix.req,group)
      df  = data.totrans[,ix]
      re  = sprintf("^(%s)(%s)",paste(ix.req,collapse="|"),group)
      names(df) = gsub(re,"\\1",names(df))
      ##-------------------------------------------------------------
      df  = tlr2xy(df,coord,...,inverse=inverse,scale=scale)[,ix.dst]
      ##-------------------------------------------------------------
      names(df) = paste(ix.dst,group,sep="")
      if(!empty(df)) data.notrans = if(!empty(data.notrans)){ cbind(data.notrans,df) }else{ df }
    }
    return(data.notrans)
  }
  
  #Local function to adjust the range, depending on if inverse or not
  adjustRange <- function(input,lim,inv=inverse){
    if(is.null(lim)) lim=c(0,1)
    if( !diff(lim) ) lim=c(0,1)
    adl   = abs(diff(lim))
    ml    = min(lim)
    input = if(inv[1]){ input*adl + ml }else{ (input-ml)/adl }
    input
  }
  
  #Local function to scale ternary coordinates
  scaleCoordinatesToUnity <- function(input){
    s  <- rowSums(input[,ix.trl]);
    ix <- which(!as.logical(s))
    if(length(ix) > 0){ input[ix,ix.trl] <- 1/3; s[ix]  <- 1.0 } #Prevent Div By 0 error
    for(x in ix.trl){ input[,x] = input[,x]/s }
    input
  }
  
  #Forward transformation is ternary to cartesian
  if(!inverse[1]){
    #Check the missing aesthetics
    .check.aes(coord,ix.trl,names(data),inverse=FALSE)
    #If scale to composition sum of 1
    if(scale[1]){ data = scaleCoordinatesToUnity(data) }
    #Adjust for the Limits.
    for(x in mapping){ 
      data[,x] = adjustRange(data[,x],coord$limits[[ mappingRev[[x]] ]]) 
    }
    #Calculate
    data$y = data[,as.character(mapping['T'])]*tan(pi/3)*0.5 
    data$x = data[,as.character(mapping['R'])]+data$y*tan(pi/6)
    data = data[,-which(names(data) == 'z')]
    
  #Inverse transformation is cartesian to ternary
  }else{
    #Check the missing aesthetics
    .check.aes(coord,ix.xy,names(data),inverse=TRUE)
    #Calculate
    out.R = data[,ix.xy[1]] - data[,ix.xy[2]]*tan(pi/6)
    out.T = data[,ix.xy[2]]/(tan(pi/3)*0.5)
    out.L = 1.0 - out.R - out.T
    #Adjust for the Limits
    for(x in mappingRev){ 
      data[, mapping[[x]] ] = adjustRange( get(sprintf('out.%s',x)) ,coord$limits[[ x ]]) 
    }
  }
  
  #Done
  data
}

#' @details \code{xy2tlr} transforms from the cartesian to ternary spaces, an inverse transformation 
#' transforms between ternary to cartesian spaces, it is the reciprocal to \code{\link{tlr2xy}}, therefore
#' an inverse transformation in \code{\link{xy2tlr}} function is the same as the forward 
#' transformation in \code{\link{tlr2xy}} 
#' @rdname ternary_transformation
#' @export
xy2tlr <- function(data,coord,...,inverse=FALSE,scale=TRUE) 
  tlr2xy(data,coord,...,inverse=!inverse,scale=scale) 


#internal
#get the set groups
.get.sets <- function(vars,cols){
  re    = sprintf("^(%s)(.*)",paste(vars,collapse="|"))
  match = grepl(re,cols,perl=TRUE)
  ix    = which(match); if(length(ix) == 0) return(NULL)
  unique(gsub(re,"\\2",cols[ix]))
}

#Get combinations
.combos = function(a,b,collapse=""){ 
  apply(expand.grid(a,b), 1, paste, collapse=collapse) 
}

#Check the aesthetics
.check.aes = function(coord,ix,colNames,inverse=FALSE){
  missing = unique(setdiff(ix,colNames))
  if(length(missing) > 0){
    dir = c('tlr','xy'); if(inverse) dir = rev(dir)
    msg = sprintf("ggtern: %s requires the following missing aesthetics (%s) : %s", 
                  class(coord)[1], paste(dir,collapse="->"), paste(missing,collapse="', '"))
    stop(msg,call.=FALSE)
  }
}