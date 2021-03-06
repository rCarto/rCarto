lgdDisplayDistr <-
function (posLeg, lblLeg, colours,  na.leg , txtCexLeg, legend,NACol) 
{
  rect.width <- 0.03
  rect.height <- 0.03
  inset <- c(0.01, 0.01)
  usr <- par("usr")
  nb.rect <- length(lblLeg)-1
  inset.x <- (usr[2] - usr[1]) * inset[1]
  inset.y <- (usr[4] - usr[3]) * inset[2]
  rect.width <- (usr[2] - usr[1]) * rect.width
  space.width <- rect.width/3
  text.width <- max(strwidth(lblLeg,cex=txtCexLeg))
  if (na.leg) {text.width <- max(c(text.width, strwidth("no data",cex=txtCexLeg)))}
  total.width <- max(c((rect.width + space.width + text.width),strwidth(legend,cex=txtCexLeg)))
  rect.height <- (usr[4] - usr[3]) * rect.height
  total.height <- ((nb.rect+1) * rect.height) + strheight("lp",cex=txtCexLeg)
  if (na.leg) {total.height <- total.height + 2 * rect.height}
  
  left <- switch(posLeg, 
                 bottomright = , topright = , 
                 right = usr[2] - total.width - inset.x, 
                 bottomleft = ,left = , 
                 topleft = usr[1] + inset.x, 
                 bottom = ,top = , 
                 center = (usr[1] + usr[2] - total.width)/2)
  
  top <- switch(posLeg, 
                bottomright = , bottom = , 
                bottomleft = usr[3] +  total.height + inset.y  , 
                topleft = ,top = , 
                topright = usr[4] - inset.y -  strheight(legend,cex=txtCexLeg), 
                left = ,right = , 
                center = (usr[3] + usr[4] + total.height)/2)
  
  text(left, top  , labels = legend, cex = txtCexLeg,adj=c(0, 0))
  rects <- 1:nb.rect
  rect(left, (top - rects * rect.height) , left + rect.width, 
       (top - (rects + 1) * rect.height) , col = rev(colours))
  text(left + rect.width + space.width, top - (1:(nb.rect + 1)) * rect.height, 
       labels = rev(lblLeg), adj = c(0, 0.5),cex = txtCexLeg)
  
  if (na.leg) {
    rect(left, top - (nb.rect + 2) * rect.height, left + 
           rect.width, top - (nb.rect + 3) * rect.height, col=NACol)
    text(left + rect.width + space.width, top - (nb.rect +2.5) * rect.height, 
         labels = "no data", adj = c(0,0.5), cex = txtCexLeg)
  }
}
