#' Position Legend in Convenient Locations
#' 
#' A convenience function to position the legend at various internal positions
#' @param x the position, valid values are topleft, middleleft, bottomleft, topright, middleright and bottomright, or the shortened versions 
#' respecitvely, tl, ml, bl, tr, mr, br
#' @author Nicholas Hamilton
#' @rdname theme_legendposition
#' @export
theme_legend_position = function(x='topleft'){
  
  if(!x %in% c(.poss,.posn) ) 
    stop(sprintf("Valid positions are: '%s'",joinCharacterSeries(c(.posn,.poss),lastWord='or')))
  
  #Make short
  pos = c(.xpos,.ypos)
  for(m in names(pos)){ x = gsub(m,pos[[m]],x) }
  
  .re.xpos = sprintf("(%s)$",paste(.xpos,collapse="|")); 
  .re.ypos = sprintf("^(%s)",paste(.ypos,collapse="|")); 
 
  ypos = gsub(.re.xpos,"",x, ignore.case = TRUE,perl = TRUE);
  xpos = gsub(.re.ypos,"\\2",x, ignore.case = TRUE)
  
  xpos = ifthenelse(xpos=='m',0.5,ifthenelse(xpos=='r',1,0))
  ypos = ifthenelse(ypos=='m',0.5,ifthenelse(ypos=='t',1,0))
  
  theme(legend.position       =c(xpos,ypos),
        legend.justification  =c(xpos,ypos),
        legend.box.just       ='left')
}

.ypos = c(top = 't', middle='m',bottom='b')
.xpos = c(left= 'l', middle='m',right='r')
.poss = apply(merge(.ypos,.xpos),1,function(x)paste0(x,collapse=''))
.posn = apply(merge(names(.ypos),names(.xpos)),1,function(x)paste0(x,collapse=''))
