#' Ternary Coordinate System
#' 
#' \code{coord_tern} is a function which creates a transformation mechanism between the ternary system, and, the cartesian system.
#' It inherits from the fixed coordinate system, employing fixed ratio between x and y axes once transformed.
#' 
#' @section Aesthetics (Required in Each Layer):
#' \Sexpr[results=rd,stage=build]{ggtern:::rd_aesthetics("coord", "tern")}
#' 
#' Abovementioned limitations include the types of geometries which can be used (ie approved geometries), 
#' or modifications to required aesthetic mappings. One such essential patch is, for approved geometries previously 
#' requiring \code{x} and \code{y} coordinates, now require an additional \code{z} coordinate, and, 
#' \code{\link{geom_segment}} goes one step further in that it requires both an additional 
#' \code{z} and \code{zend} coordinate mappings. 
#' 
#' In essence, the required aesthetics are the product between what
#' is required of each 'layer' and what is required of the 'coordinate system'.
#' @param Tlim the range of T in the ternary space
#' @param Llim the range of L in the ternary space
#' @param Rlim the range of R in the ternary space
#' @inheritParams ggplot2::coord_cartesian
#' @return \code{coord_tern} returns a CoordTern ggproto
#' @rdname coord_tern
#' @author Nicholas Hamilton
#' @export
coord_tern <- function(Tlim = NULL, Llim = NULL, Rlim = NULL, expand = TRUE){
  rs      = CoordTern$required_scales
  ra      = CoordTern$required_aes
  mapping = sapply(rs,function(x) getOption(sprintf('tern.default.%s',x)) )
  if(!all(ra %in% as.character(mapping)))
    stop(sprintf("Options for %s are %s, must be assigned and NOT duplicated, alter the 'tern.default.X' (X=%s) global option",
                 joinCharacterSeries(rs,'and'),
                 joinCharacterSeries(ra,'and'),
                 joinCharacterSeries(rs,'or')),call.=FALSE)
  ggproto(NULL, CoordTern,
          mapping         = as.list(mapping),
          limits          = list(x = c(0,1), y = c(0,1)*.ratio(), T=Tlim, L=Llim, R=Rlim),
          ratio           = 1,
          expand          = expand,
          scales          = list(),
          labels_coord    = list(),
          theme           = theme_get()
  )
}

#' @rdname coord_tern
#' @name coord_tern
#' @usage NULL
#' @format NULL
#' @export
CoordTern <- ggproto("CoordTern", CoordCartesian,
  required_aes    = c("x","y","z"),
  required_scales = c("T","L","R"),
  aspect          = function(self, ranges) { 
    (diff(ranges$y.range) / diff(ranges$x.range)) * self$ratio 
  },
  transform       = function(self, data, panel_params){
    
    #Variables and functions.
    ix               = c('x','y')
    angle            = .theme.get.rotation(self)
    addOrigin        = function(d,ix,o){  d[,ix] = t(t(d[,ix,drop=FALSE]) + o); d }
    data             = tlr2xy(data,self)
    coordShift       = sapply(c('tern.axis.hshift','tern.axis.vshift'),function(x){ calc_element(x,self$theme) %||% 0})
    
    #For each coordinate group, conduct the re-centering, translation / rotation process
    for(group in .get.sets(ix,names(data)) ){
      ix.comb        = .combos(ix,group)
      
      #Run check to see if all the required columns are present within the group.
      #A required column is the cartesian product of the group and the required index, so it could be (xend,yend), (xstart,ystart), etc...
      #This shouldn't be a problem, since the same check is executed during the master transformation in the tlr2xy(...) operation above
      if(!all(ix.comb %in% names(data))) stop(sprintf("Problem with proposed tranlation/rotation, require columns (%s) wich are not present.",
                                                      joinCharacterSeries(setdiff(ix.comb,names(data)),lastWord='and') ))
      
      #Determine Origin
      xtrm           = tlr2xy(.get.tern.extremes(self,panel_params,FALSE),self)
      origin         = apply(xtrm[,ix],2,function(x){mean(x)}) ##ORIGIN TO BE CENTROID
      
      #Any multiple of 360 degrees can be disregarded
      if(angle %% 360 != 0){
        #Translation   
        data           = addOrigin(data,ix.comb,-origin); 
        xtrm           = addOrigin(xtrm,ix,     -origin)
        #Rotation  
        data[,ix.comb] = .rotation(data[,ix.comb],angle) 
        xtrm[,ix]      = .rotation(xtrm[,ix],     angle)
        #Translate Back
        data           = addOrigin(data,ix.comb,+origin)
        xtrm           = addOrigin(xtrm,ix,     +origin)
      }
      
      #Re-Center
      origin         = apply(xtrm[,ix],2,function(x){ mean(range(x))} ) ##ORIGIN TO BE MEDIAN
      target         = c(mean(panel_params$x.range),mean(panel_params$y.range)) + as.numeric(coordShift)
      data           = addOrigin(data,ix.comb,(target - origin))
    }
    
    #self$super$super$transform(data,panel_params)
    self$super()$super()$transform(data,panel_params)
  },
  render_axis_h   = function(panel_params, theme){
    list(
      top = zeroGrob(),
      bottom = zeroGrob()
    )
  },
  render_axis_v   = function(panel_params, theme){
    list(
      left = zeroGrob(),
      right = zeroGrob()
    )
  },
  
  render_bg       = function(self,panel_params, theme){
    items = list() 
    extrm = .get.tern.extremes(self,panel_params)
    items = .render.background(self,extrm,theme,items)
    if(!.theme.get.gridsontop(theme)){
      items = .render.grid(self,extrm,panel_params,theme,items)
      if(!.theme.get.showmask(theme))
        items = .render.fgset(self,extrm,panel_params,theme,items)
    }
    gTree(children = do.call("gList",items))
  },
  
  render_fg     = function(self,panel_params, theme){
    items = list() 
    extrm = .get.tern.extremes(self,panel_params)
    if(.theme.get.gridsontop(theme)){
      items = .render.grid( self,extrm,panel_params,theme,items)
      items = .render.fgset(self,extrm,panel_params,theme,items)
    }
    items = .render.titles(self,extrm,panel_params,theme,items)
    gTree(children = do.call("gList", items))
  },
  
  remove_labels = function(self,table){
    #Determine the Layout
    layout <- table$layout
    #Remove Y-axis columns
    ycols  <- layout[grepl("^ylab-l", layout$name), , drop = FALSE]
    table  <- table[,-ycols$l]
    #Remove X-axis Rows
    xrows  <- layout[grepl("^xlab-b", layout$name), , drop = FALSE]
    table  <- table[-xrows$t,]
    table
  },
  
  setup_panel_params = function(self, scale_x, scale_y, params = list() ) {
    
    train_cartesian <- function(panel_params,limits,name,continuousAmount) {
      if (self$expand) {
        # expand <- ggint$expand_default(panel_params,continuous=continuousAmount)
        # expand = ggplot2:::expand_limits_continuous(limits, continuousAmount, panel_params)
        # expand <- c(continuousAmount, continuousAmount)
        expand <- c(0, 0)
      } else {
        expand <- c(0, 0)
      }
      print(limits)
      if (is.null(limits)) {
        range <- panel_params$dimension(expand)
      } else {
        range <- range(panel_params$transform(limits))
        range <- expand_range(range, expand[1], expand[2])
      }
      
      out <- panel_params$break_info(range)
      names(out) <- paste(name, names(out), sep = ".")
      out
    }
    
    #Determine the epansiion amount
    expand.amount = calc_element('tern.panel.expand',theme=self$theme)
    
    #Adjust for rotation
    extremes        = .get.tern.extremes(self,list(x.range=self$limits$x, y.range=self$limits$y,x.rescale=FALSE,y.rescale=FALSE))
    extremes        = extremes[,c('x','y')]
    currentMidpoint = c(mean(self$limits$x),mean(self$limits$y))
    ternaryMidpoint = apply(extremes,2,function(x){mean(range(x))})
    shift           = 0.5*(currentMidpoint - ternaryMidpoint)
    
    #Execute the Training
    c(
      # train_cartesian(scale_x, self$limits$x - shift[1],"x",c(expand.amount,0) ),
      # train_cartesian(scale_y, self$limits$y - shift[2],"y",c(expand.amount,0) )
      view_scales_from_scale(scale_x, self$limits$x, TRUE, c(expand.amount,0)),
      view_scales_from_scale(scale_y, self$limits$y, TRUE, c(expand.amount,0))
    )
  }
)

# Inspired by ggplot2
view_scales_from_scale <- function(scale, coord_limits = NULL, expand = TRUE, expansion) {
  # expansion <- default_expansion(scale, expand = expand)
  limits <- scale$get_limits()
  continuous_range <- ggint$expand_limits_scale(scale, expansion, limits, coord_limits = coord_limits)
  aesthetic <- scale$aesthetics[1]
  
  view_scales <- list(
    ggint$view_scale_primary(scale, limits, continuous_range),
    sec = ggint$view_scale_secondary(scale, limits, continuous_range),
    arrange = scale$axis_order(),
    range = continuous_range
  )
  names(view_scales) <- c(aesthetic, paste0(aesthetic, ".", names(view_scales)[-1]))
  
  view_scales
}

#----------------------------------------------------------------------------------
#Internals >>>> Ratio
#----------------------------------------------------------------------------------
.ratio = function(){ 0.5*tan(60*pi/180) }

#----------------------------------------------------------------------------------
#Internals >>>> Ternary Extremes
#----------------------------------------------------------------------------------
.get.tern.extremes <- function(self,panel_params,transform=TRUE){
  ex = .check.tern.extremes(self)
  if(transform) ex = self$transform(ex,panel_params)
  ex
}

.check.tern.extremes <- function(self,precision=7){
  ix           = self$required_scales
  scales       = self$scales[ix]
  limitsfunc   = Vectorize(function(r,c){ do.call(if(r == c)'max'else'min',list( scales[[ ix[c] ]]$limits %||% self$limits[[ ix[c] ]] %||% c(0,1) ))  })
  ex           = as.data.frame(outer(1:3,1:3,limitsfunc))
  colnames(ex) = as.character(self$mapping[ix])
  sums         = round(apply(ex,1,sum),precision)
  if(!all(sums == 1.0)){
    colnames(ex) = ix; ex$Sum = sums; print(ex)
    stop("Invalid Ternary Limits, Each Point Must Sum to Unity...",call.=FALSE) 
  }
  invisible(ex)
}

#----------------------------------------------------------------------------------
#Internals >>>> ANGLES
#Functions to determine the rotation angles for the various components
#----------------------------------------------------------------------------------
.get.angles             <- function(clockwise){ c(0,120,240) - as.logical(clockwise)*180 }
.get.angles.arrows      <- function(clockwise){ .get.angles(clockwise) + if(clockwise){-30}else{30} }
.get.angles.arrowmarker <- function(clockwise){ x = c(60,0,-60); if(clockwise){x}else{x[c(3,1:2)]}}
.get.angles.ticklabels  <- function(clockwise){ c(0,-60,60) }


#----------------------------------------------------------------------------------
#Internals >>>> Theme flags.
#----------------------------------------------------------------------------------
.theme.get.clockwise <- function(theme){
  clockwise = calc_element('tern.axis.clockwise',theme)
  ifthenelse(is.logical(clockwise),clockwise[1],getOption("tern.clockwise"))
}
.theme.get.gridsontop <- function(theme){
  ret = calc_element('tern.panel.grid.ontop',theme)
  ifthenelse(is.logical(ret),ret[1],FALSE)
}

.theme.get.bordersontop <- function(theme){
  ret = calc_element('tern.axis.line.ontop',theme)
  ifthenelse(is.logical(ret),ret[1],FALSE)
}

.theme.get.showtitles <- function(theme){
  showtitles = calc_element("tern.axis.title.show",theme=theme)
  ifthenelse(is.logical(showtitles),showtitles[1],getOption("tern.title.show"))
}
.theme.get.showlabels <- function(theme){
  showlabels = calc_element("tern.axis.text.show",theme=theme)
  ifthenelse(is.logical(showlabels),showlabels[1],getOption("tern.text.show"))
}
.theme.get.showgrid.major <- function(theme){
  showgrid   = calc_element("tern.panel.grid.major.show",theme=theme)
  ifthenelse(is.logical(showgrid),showgrid[1],getOption("tern.grid.major.show"))
}
.theme.get.showgrid.minor <- function(theme){
  showgrid   = calc_element("tern.panel.grid.minor.show",theme=theme)
  ifthenelse(is.logical(showgrid),showgrid[1],getOption("tern.grid.minor.show"))
}
.theme.get.outside       <- function(theme){
  outside     = calc_element("tern.axis.ticks.outside",theme=theme)
  ifthenelse(is.logical(outside),outside[1],getOption("tern.ticks.outside"))
}
.theme.get.showprimary   <- function(theme){
  showprimary = calc_element("tern.axis.ticks.primary.show",theme=theme)
  ifthenelse(is.logical(showprimary), showprimary[1],getOption("tern.ticks.primary.show"))
}
.theme.get.showsecondary <- function(theme){
  showsecondary = calc_element("tern.axis.ticks.secondary.show",theme=theme)
  ifthenelse(is.logical(showsecondary),showsecondary[1],getOption("tern.ticks.secondary.show"))
}
.theme.get.showarrows <- function(theme){
  showarrows   = calc_element('tern.axis.arrow.show',theme=theme)
  ifthenelse(is.logical(showarrows),showarrows[1],getOption("tern.arrow.show"))
}
.theme.get.showmask <- function(theme){
  showmask = calc_element('tern.panel.mask.show',theme=theme)
  ifthenelse(is.logical(showmask),showmask[1],getOption('tern.mask.show'))
} 

.theme.get.label <- function(self,n,d=n,suffix=''){
  x      = function(n,s) sprintf("%s%s",n,s)
  if(!is.character(n)) return('')
  labels = self$labels_coord
  ix     = unique(c(x(n,suffix),x(self$mapping[[n]],suffix),n,self$mapping[[n]])) 
  id     = which(ix %in% names(labels))
  if(length(id) == 0) d else labels[[ix[id[1]]]]
}
.theme.get.rotation <- function(self){
  tryCatch({
    angle = calc_element('tern.panel.rotate',self$theme)
    return(ifthenelse(is.finite(angle),angle,0)[1])
  },error=function(e){ 
    warning(e) 
  })
  return(0)
}

#----------------------------------------------------------------------------------
#Internals >>>> Render Components
# -Backgrounds
# -Borders
# -Grids
# -Precession Arrows & Markers
# _Apex Titles
#----------------------------------------------------------------------------------
.get.grid.data <- function(self,theme,data.extreme,X,major=TRUE,angle=0,angle.text=0){
  clockwise   = .theme.get.clockwise(theme)
  seq.tlr     = self$required_scales
  ix          = which(X == seq.tlr)
  existing    = data.frame()
  
  tryCatch({
    scale = self$scales[[X]]
    
    #Determine the limits
    limits = (if(inherits(scale$limits,'waiver')) NULL else scale$limits) %||% c(0,1)
    
    #Determine the Breaks
    breaks <- if(major) scale$breaks else scale$minor_breaks
    if(inherits(breaks,'waiver')){
      breaks = breaks_tern(limits = limits, isMajor = major)
    }
    
    #Bypass if necessary
    if(length(breaks) == 0) 
      return(existing)
    
    #Determine the labels
    labels <- if(major) (scale$labels %||% waiver()) else ""
    if(inherits(labels,'waiver'))
      labels = labels_tern(limits=limits,breaks=breaks)
    
    #major ticklength
    tl.major <- 0
    tryCatch({
      tl.major <- convertUnit(theme$tern.axis.ticks.length.major,"npc",valueOnly=T)
    },error=function(e){ warning(e) })
    
    #minor ticklength
    tl.minor <- 0
    tryCatch({
      tl.minor <- convertUnit(theme$tern.axis.ticks.length.minor,"npc",valueOnly=T)
    },error=function(e){  warning(e) })
    
    #Assign new id.
    id     <- (max(existing$ID,0) + 1)
    ix     <- min(ix,ifthenelse(major,length(tl.major),length(tl.minor)))
    majmin <- if(major) 'major' else 'minor' #Major or Minor Element Name part.
    
    #The new dataframe
    new            <- data.frame(ID = id,Scale = X, breaks, Labels = labels, Major = major)
    new            <- subset(new, breaks >= min(limits) & breaks <= max(limits))
    new$Prop       <- (new$breaks - min(limits)) / abs(diff(limits))
    new$TickLength <- ifthenelse(major,tl.major[ix],tl.minor[ix])
    new$NameText   <- sprintf("tern.axis.text.%s",X)
    new$NameTicks  <- sprintf("tern.axis.ticks.%s.%s",majmin,X)
    new$NameGrid   <- sprintf("tern.panel.grid.%s.%s",majmin,X)
    new$Major      <- major
    
    ##Start and finish positions of scale.
    out            <- c("x","y")
    
    #Start indexes.
    ix.s           <- which(seq.tlr == X);
    finish         <- as.numeric(data.extreme[ix.s,out])
    
    #For Ticks
    ix.f           <- ifthenelse(clockwise,if(ix.s == 3){1}else{ix.s+1},if(ix.s == 1){3}else{ix.s-1})
    start          <- as.numeric(data.extreme[ix.f,out])
    for(i in 1:length(out))
      new[,out[i]] <- new$Prop*(finish[i]-start[i]) + start[i]
    
    #FOR GRID
    ix.f           <- ifthenelse(clockwise,if(ix.s == 1){3}else{ix.s-1},if(ix.s == 3){1}else{ix.s+1})
    start          <- as.numeric(data.extreme[ix.f,out])
    for(i in 1:length(out))
      new[,paste0(out[i],"end.grid")] <- new$Prop*(finish[i]-start[i]) + start[i]
    
    #The tick angles.
    new$Angle      <- angle + .theme.get.rotation(self)
    new$Angle.Text <- .valid.angle(angle.text)
    
    #Determine the tick finish positions for segments.
    new$xend       <- cos(new$Angle*pi/180)*new$TickLength + new$x
    new$yend       <- sin(new$Angle*pi/180)*new$TickLength + new$y
    
    #Determine the secondary tick start and finish positions.
    new$x.sec      <- new$xend.grid
    new$y.sec      <- new$yend.grid
    new$xend.sec   <- cos((new$Angle+180)*pi/180)*new$TickLength + new$x.sec
    new$yend.sec   <- sin((new$Angle+180)*pi/180)*new$TickLength + new$y.sec
    
    return(new)
    
  },error=function(e){
    warning(e)
  })
  
  return(existing)
}

.render.fgset <- function(self,data.extreme,panel_params,theme,items){
  items = .render.ticks(      self,data.extreme,panel_params,theme,items)
  items = .render.border.main(self,data.extreme,              theme,items)
  items = .render.border.axis(self,data.extreme,              theme,items)
  items = .render.labels(     self,data.extreme,panel_params,theme,items)
  items = .render.arrows(     self,data.extreme,panel_params,theme,items)
  items
}

.render.background <- function(self,data.extreme,theme,items){
  tryCatch({
      e <- calc_element('tern.panel.background',theme=theme,verbose=F)
      if(inherits(e, "element_blank"))
        return(items)
      
      g <- polygonGrob( data.extreme$x, 
                              data.extreme$y, 
                              default.units = "npc",
                              id   = rep(1,nrow(data.extreme)),
                              gp   = gpar(  col  = e$colour %||% 'transparent',
                                            fill = alpha(e$fill %||% 'transparent',ifthenelse(!is.numeric(e$alpha),1,e$alpha)),
                                            lwd  = 0, #ifthenelse(!is.numeric(e$size),0,e$size)*find_global_tern(".pt"),
                                            lty  = e$linetype
                              )
      )
      items[[length(items) + 1]] <- g
  },error = function(e){ 
    warning(e) 
  })
  items
}

.render.border.main <- function(self,data.extreme,theme,items){
  tryCatch({
    #e      = calc_element('tern.panel.background',theme,verbose=F)
    e      = calc_element('panel.border',theme,verbose=F)
    if(inherits(e, "element_blank"))
      return(items)
    ex     = rbind(data.extreme,data.extreme[1,])
    grob   = ggint$element_grob.element_line(e,size=e$size,x=ex$x,y=ex$y)
    items[[length(items) + 1]] = grob
  },error=function(e){ 
    warning(e)
  })
  items
}

.render.border.axis <- function(self,data.extreme,theme,items,X=self$required_scales){
  
  #Only Unique Entries
  X = unique(X)
  
  #Checks
  seq.tlr = self$required_scales
  if(any(!{X %in% seq.tlr})) stop('Invalid X')
  
  grobs = function(name,s,f,items){
    tryCatch({
      grob = element_render(theme,name, x = data.extreme$x[c(s,f)], y = data.extreme$y[c(s,f)])
      items[[length(items) + 1]] <- grob
    },error=function(e){ 
      #warning(e) 
    })
    items
  }
 
  f = if(.theme.get.clockwise(theme)) c(2,3,1) else c(3,1,2)
  for(x in X){
    s      = which(seq.tlr == x)
    name   = sprintf("tern.axis.line.%s",seq.tlr[s])
    items  = grobs(name,s,f[s],items)
  }
  items
}

.render.ticks  <- function(self,data.extreme,panel_params,theme,items,X=self$required_scales){
  
  #Only Unique Entries
  X = unique(X)
  
  #Checks
  seq.tlr = self$required_scales
  if(any(!{X %in% seq.tlr})) stop('Invalid X')

  outside     = .theme.get.outside(theme)
  clockwise   = .theme.get.clockwise(theme)
  primary     = .theme.get.showprimary(theme)
  secondary   = .theme.get.showsecondary(theme)
  angle       = .get.angles(clockwise) + (!outside)*180
  angle.text  = .get.angles.ticklabels(clockwise) + .theme.get.rotation(self)
  
  #Function to render the grobs
  grobs  <- function(name,items,df,primary=TRUE){
    tryCatch({  
      append = function(x) paste0(x,if(primary) NULL else '.sec')
      ix.x   = append(c('x','xend'))
      ix.y   = append(c('y','yend'))
      
      e = calc_element(name,theme)
      if(inherits(e, "element_blank"))
        return(items)
      
      
      g = lapply(1:nrow(df),function(ix){ 
        segmentsGrob(x0 = df[ix,ix.x[1]], 
                     x1 = df[ix,ix.x[2]], 
                     y0 = df[ix,ix.y[1]], 
                     y1 = df[ix,ix.y[2]],
                     default.units ="npc",
                     arrow         = NULL,
                     gp            = gpar(col     = e$colour %||% 'transparent', 
                                          lty     = e$linetype,
                                          lineend = 'butt',
                                          lwd     = e$size %||% 0)
        )
      })
      
      items = c(items,g)
    },error = function(e){ 
      warning(e) 
    })
    items
  }
  
  #Iterate over the values of X
  for(x in X){
    
    #Determine the index
    ix = which(seq.tlr == x)
    
    #Generate the data
    df = ldply(c(T,F),function(major){
      .get.grid.data(self,theme,data.extreme,X = x, major = major, angle = angle[ix], angle.text = angle.text[ix])
    })
    
    #If Primary Ticks
    if(primary | secondary){
      for(name in unique(df$NameTicks)){
        df.sub = df[which(df$NameTicks == name),,drop = F]
        if(primary)   items = grobs(name = name, items = items, df = df.sub, primary = T)
        if(secondary) items = grobs(name = name, items = items, df = df.sub, primary = F)
      }
    }
  }
  
  #Done
  items
}

.render.labels <- function(self,data.extreme,panel_params,theme,items,X=self$required_scales){
  #Only Unique Entries
  X = unique(X)
  
  #Checks
  seq.tlr = self$required_scales
  if(any({!X %in% seq.tlr})) 
    stop('Invalid X')

  #Axis labels, ie labels next to ticks
  grobs <- function(name,items,df,outside,showprimary){ 
    tryCatch({
      df        = df[which(df$Labels != ''),]
      e         = calc_element(name,theme,verbose=FALSE)
      
      if(empty(df) || inherits(e, "element_blank"))
        return(items)
      
      latex     = calc_element('tern.plot.latex',theme)
      xts       = if(outside) df$x else df$xend
      xtf       = if(outside) df$xend else df$x
      yts       = if(outside) df$y else df$yend
      ytf       = if(outside) df$yend else df$y 
      angle     = is.numericor(e$angle,0) + is.numericor(unique(df$Angle.Text)[1],0)
      dA        = angle - atan2(ytf - yts, xtf - xts)*180/pi #DEGREES, Angle Difference between Ticks and Labels
      adj       = as.numeric(!outside)
      
      g = textGrob(
        label   = label_formatter(df$Labels,latex=latex),
        x       = ifthenelse(showprimary || !outside,xtf,xts) + convertX(cos(pi*(df$Angle/180 + adj))*unit(2,'pt'),'npc',valueOnly = T),
        y       = ifthenelse(showprimary || !outside,ytf,yts) + convertY(sin(pi*(df$Angle/180 + adj))*unit(2,'pt'),'npc',valueOnly = T),
        hjust   = +0.5*cos(pi*(dA/180 - 1)) + is.numericor(e$hjust,0), #BACK TO RADIANS
        vjust   = -0.5*sin(pi*(dA/180 - 1)) + is.numericor(e$vjust,0), #BACK TO RADIANS
        rot     = angle,
        default.units = 'npc',
        gp  = gpar(
          col        = e$colour %||% 'transparent',
          fontsize   = e$size %||% 0,
          fontfamily = e$family,
          fontface   = e$fontface,
          lineheight = e$lineheight
        )
      )
      
      items[[length(items) + 1]] <- g
    },error = function(e){ 
      warning(e) 
    })
    items
  }
  
  if(.theme.get.showlabels(theme)){
    outside     = .theme.get.outside(theme)
    showprimary = .theme.get.showprimary(theme)
    clockwise   = .theme.get.clockwise(theme)
    angle       = .get.angles(clockwise) + (!outside)*180
    angle.text  = .get.angles.ticklabels(clockwise) + .theme.get.rotation(self)
    #Generate the data
    df = ldply(X,function(x){
      ix =  which(seq.tlr == x)
      .get.grid.data(self,theme,data.extreme,X = x, major = TRUE, angle = angle[ix], angle.text = angle.text[ix])
    })
    for(name in unique(df$NameText))
      items = grobs(name=name,items=items,df=df[which(df$NameText  == name),,drop=F],outside,showprimary)
  }
  items
}

.render.grid <- function(self,data.extreme,panel_params,theme,items,X=self$required_scales){
  
  #Only Unique Entries
  X = unique(X)
  
  #Checks
  seq.tlr = self$required_scales
  if(any({!X %in% seq.tlr})) 
    stop('Invalid X')
  
  #Process the flags.
  clockwise     <- .theme.get.clockwise(theme)
  outside       <- .theme.get.outside(theme)
  showgrid.major<- .theme.get.showgrid.major(theme)
  showgrid.minor<- .theme.get.showgrid.minor(theme)
  
  #Get the Angles
  angle      <- .get.angles(clockwise) + (!outside)*180
  angle.text <- .get.angles.ticklabels(clockwise) + .theme.get.rotation(self)
  
  ##get the base data.
  df = expand.grid(ix=seq_along(seq.tlr),major=c(T,F))
  df = ddply(df,c('major','ix'),function(x){
    .get.grid.data(self,theme,data.extreme,X = seq.tlr[x$ix], major = x$major, angle = angle[x$ix], angle.text = angle.text[x$ix])
  })
  
  #If Nothing in 'd', return the curent list of items
  if(empty(df))
    return(items)
  
  grobs   <- function(name,items,df,showgrid.major=TRUE,showgrid.minor=TRUE){
    if((unique(df$Major) & showgrid.major) | (!unique(df$Major) & showgrid.minor)){
      tryCatch({
        e = calc_element(name,theme)
        g = lapply(1:nrow(df),function(ix){ 
          segmentsGrob(x0 = df$x[ix], 
                       x1 = df$xend.grid[ix], 
                       y0 = df$y[ix], 
                       y1 = df$yend.grid[ix],
                       default.units ="npc",
                       arrow         = NULL,
                       gp            = gpar(col     = e$colour %||% 'transparent', 
                                            lty     = e$linetype,
                                            lineend = 'butt',
                                            lwd     = (e$size %||% 0)*.pt)
          )
        })
        items = c(items,g)
      },error = function(e){ 
        warning(e) 
      })
    }
    items
  }
  
  #PROCESS TICKS AND LABELS
  if(showgrid.major | showgrid.minor){
    for(name in unique(df$NameGrid)){ 
      items <- grobs(name=name, items=items, df = df[ which(df$NameGrid  == name),,drop=FALSE], 
                     showgrid.major = showgrid.major, showgrid.minor = showgrid.minor)
    } 
  }
  
  items
}


.render.titles <- function(self,data.extreme,panel_params,theme,items){
  if(!.theme.get.showtitles(theme)) 
    return(items)
  
  #Determine the required scales
  seq.tlr = self$required_scales
  
  #Determine the length of the side
  sidelength = sqrt( diff(data.extreme$x[1:2])^2 + diff(data.extreme$y[1:2])^2)
  
  #Build the local function for building the grobs
  grobs = function(name,ix,items){
    tryCatch({
      
      e = calc_element(name,theme=theme,verbose=F)
      if(inherits(e, "element_blank"))
          return(items)
      
      ixc   <- c('x','y')
      latex <- calc_element('tern.plot.latex',theme)
      point <- as.numeric(data.extreme[ix,ixc])
      base  <- as.numeric(apply(data.extreme[-ix,ixc],2,mean))
      angle <- atan2((point[2]-base[2])*.ratio(),point[1]-base[1])
      n     <- regmatches(name,regexpr(".$",name)) 
      label <- c(self$scales[[n]]$name, self$labels_coord[[n]], self$labels_coord[[ self$mapping[[n]] ]], n)[1]
      grob  <- element_render(theme, name,
                              label = label_formatter(label,latex=latex),
                              x     = data.extreme$x[ix] + 0.05*sidelength*cos(angle),
                              y     = data.extreme$y[ix] + 0.05*sidelength*sin(angle),
                              hjust = e$hjust - 0.5*cos(angle),
                              vjust = e$vjust - 0.5*sin(angle))
      items[[length(items) + 1]] <- grob
    },error = function(e){
      #warning(e)
    })
    items
  }
  
  #process the axes
  for(ix in seq_along(seq.tlr))
    items = grobs(sprintf("tern.axis.title.%s",seq.tlr[ix]),ix,items)
  
  #Done, Return the list of items
  items
}

# The Arrows Parallel to the Axes
.render.arrows <- function(self,data.extreme,details,theme,items){
  
  if(!.theme.get.showarrows(theme))
    return(items)
  
  tryCatch({
    
    #clockwise or anticlockwise precession
    clockwise <- .theme.get.clockwise(theme)
    
    #The basic data.
    d.s <- data.extreme[ifthenelse(clockwise,c(2,3,1),c(3,1,2)),c('x','y')]
    d.f <- data.extreme[c(1,2,3),c('x','y')]
    rownames(d.s) <- rownames(d.f) #Correct rownames
    
    #Determine the length of the side
    sidelength = sqrt( diff(data.extreme$x[1:2])^2 + diff(data.extreme$y[1:2])^2)
    
    #arrow start and finish proportions
    arrowstart = calc_element('tern.axis.arrow.start', theme)
    arrowfinish= calc_element('tern.axis.arrow.finish',theme)
    
    #Ensure arrow start and finish length is 3.
    if(length(arrowstart) != 3 && length(arrowstart) >= 1)
      arrowstart <- rep(arrowstart[1],3)
    if(length(arrowfinish) != 3 && length(arrowfinish) >= 1)
      arrowfinish <- rep(arrowfinish[1],3)
    
    #Itterate over indexes 1:3
    for(i in c(1:3)){
      #Put in correct order.
      if(arrowfinish[i] < arrowstart[i]){
        warning(paste("Arrow size theme 'element tern.axis.arrow.finish[",i,"]' (",arrowfinish[i],") is < 'tern.axis.arrow.start[",i,"]' (",arrowstart[i],"), values will be swapped.",sep=""),call.=FALSE)
        #swapvalues
        tmp  = arrowstart[i]; arrowstart[i]  = arrowfinish[i]; arrowfinish[i] = tmp
      }
      #Check finish
      if(arrowfinish[i] > 1.0){
        warning(paste("Arrow size theme 'element tern.axis.arrow.finish[",i,"]' (",arrowfinish[i],") is > 1.0 and will be truncated",sep=""),call.=FALSE)
        arrowfinish[i] = 1.0
      }
      #Check start
      if(arrowstart[i] < 0.0){
        warning(paste("Arrow size theme 'element tern.axis.arrow.start[",i,"]' (",arrowstart[i],") is < 0.0 and will be truncated",sep=""),call.=FALSE)
        arrowstart[i] = 0.0
      }
    }
    
    #Cut down to relative proportion.
    dx   = (d.f - d.s)
    d.f  = d.f - (1 - arrowfinish)*dx
    d.s  = d.s +      arrowstart*dx
    d    = rbind(d.s,d.f)
    
    #Determine the start and end positions
    ixseq <- names(self$mapping)
    ixrow <- paste0("AT.",ixseq)
    ixcol <- c("x","y","xend","yend")
    ix    <- which(colnames(d) %in% ixcol[c(1:2)])
    d     <- cbind(d[1:3,ix],d[4:6,ix]);
    rownames(d) <- ixrow; colnames(d) <- ixcol
    
    #The arrow seperation in npc units.
    arrowsep      <- calc_element("tern.axis.arrow.sep",theme=theme,verbose=F)
    ticklength    <- max(calc_element("tern.axis.ticks.length.major",theme=theme,verbose=F),
                         calc_element("tern.axis.ticks.length.minor",theme=theme,verbose=F))
    
    #Ensure there are EXACTLY 3 values for each metric
    if(length(arrowsep)   != 3 && length(arrowsep)   >= 1){ arrowsep   = rep(arrowsep[1],3)   }
    if(length(ticklength) != 3 && length(ticklength) >= 1){ ticklength = rep(ticklength[1],3) }
    
    #Determine the Angles
    d[ixrow,"angle"]    <- .get.angles.arrows(clockwise)
    
    #get set of 3 arrowsep positions
    d[ixrow,"arrowsep"] <- arrowsep*sidelength
    
    #MOVE the Arrows Off the Axes.
    rotation = .theme.get.rotation(self)
    d[,ixcol[c(1,3)]]   <- d[,ixcol[c(1,3)]] + cos(pi*(d$angle + rotation)/180)*d$arrowsep #xcoordinates
    d[,ixcol[c(2,4)]]   <- d[,ixcol[c(2,4)]] + sin(pi*(d$angle + rotation)/180)*d$arrowsep #ycoorinates
    
    #Centerpoints, labels, arrowsuffix
    d$xmn = rowMeans(d[,ixcol[c(1,3)]])
    d$ymn = rowMeans(d[,ixcol[c(2,4)]])
    d$L   = unlist(lapply(ixseq,function(n){ .theme.get.label(self,n)  }))
    d$LA  = unlist(lapply(ixseq,function(n){ 
      #c(self$labels_coord[[ sprintf('%sarrow',n) ]],)[1]
      .theme.get.label(self,n,suffix='arrow')
    }))
    d$W   = unlist(lapply('W',function(n){  .theme.get.label(self,n,'')  }))
    d$A   = .get.angles.arrowmarker(clockwise)
    d$AL  = .valid.angle(d$A + .theme.get.rotation(self))
    
    ##Function to create new arrow grob
    .render.arrow <- function(name,ix,items){
      tryCatch({  
        e = calc_element(name,theme=theme,verbose=F)
        if(inherits(e, "element_blank"))
          return(items)
        
        g = segmentsGrob(x0 = d$x[ix], x1 = d$xend[ix], y0 = d$y[ix], y1 = d$yend[ix],
                            default.units ="npc",
                            arrow         = e$lineend,
                            gp            = gpar(col     = e$colour %||% 'transparent', 
                                                 lty     = e$linetype,
                                                 lineend = 'butt',
                                                 lwd     = e$size)
        )
        items[[length(items) + 1]] <- g
      },error = function(e){
        warning(e)
      })
      items
    }
    
    #Function to greate new label grob
    .render.label <- function(name,ix,items){
      tryCatch({  
        e    = calc_element(name,theme=theme,verbose=F)
        if(inherits(e, "element_blank"))
          return(items)
        
        latex = calc_element('tern.plot.latex',theme)
        dA    = e$angle + d$AL - 180*(atan2((d$yend - d$y)*.ratio(), d$xend - d$x)/pi + as.numeric(clockwise))
        g     = textGrob( label = arrow_label_formatter(d$LA[ix],d$W[ix],latex=latex), 
                         x     = d$xmn[ix] + convertX(cos(pi*(d$angle[ix] + rotation)/180)*unit(2,'pt'),'npc',valueOnly = T), 
                         y     = d$ymn[ix] + convertY(sin(pi*(d$angle[ix] + rotation)/180)*unit(2,'pt'),'npc',valueOnly = T), 
                         hjust = e$hjust + 0.5*sin(dA[ix]*pi/180), 
                         vjust = e$vjust + 0.5*cos(dA[ix]*pi/180),
                         rot   = d$AL[ix] + e$angle, 
                         default.units="npc", 
                         gp   = gpar(col        = e$colour %||% 'transparent', 
                                     fontsize   = e$size,
                                     fontfamily = e$family, 
                                     fontface   = e$face, 
                                     lineheight = e$lineheight))
        items[[length(items) + 1]] <- g
      },error = function(e){ 
        warning(e) 
      })
      items
    }
  
    #process the axes
    for(i in 1:length(ixseq)){
      items <- .render.arrow(paste0("tern.axis.arrow.",     ixseq[i]),i,items) #Arrows
    }
    for(i in 1:length(ixseq)){
      items <- .render.label(paste0("tern.axis.arrow.text.",ixseq[i]),i,items) #Markers
    }
    
  },error=function(e){
    message(e)
  })
  items
}


.rotation = function (xy, angle, degrees=TRUE) {
  if(degrees) angle = pi*angle/180
  xy <- as.matrix(xy)
  ca <- cos(angle)
  sa <- sin(angle)
  xy.rot <- xy %*% t(matrix(c( ca, sa, 
                              -sa, ca), 2, 2))
  return(xy.rot)
}

.valid.angle = function(x){
  if(length(x) > 1)
    return(sapply(x,.valid.angle))
  if(x >  90) x = .valid.angle(x - 180)
  if(x <=-90) x = .valid.angle(x + 180)
  x
}

