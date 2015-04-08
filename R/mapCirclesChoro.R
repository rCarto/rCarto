#' @title Proportional Circles Colored by a Discretized Quantitative Variable
#' @name mapCirclesChoro
#' @description This function represents two variables through a proportional circles map.  The areas of the circles are proportional to values of a quantitative variable and their colors reflect the discretization of an other numeric variable.
#' @details The circles are centered on the centroids of the polygons.\cr Only the six first arguments are compulsory.
#' @return A map is displayed in the graphic window.\cr You can export the map in raster or vector format (pdf).
#' @param shpFile Path to a polygon shapefile.
#' @param shpId Unique identifier of the shapefile. It must correspond to \code{dfId}.
#' @param df Data frame that contains the variables to be mapped.
#' @param dfId Unique identifier of the data frame. It must correspond to \code{shpId}.
#' @param var Name of the positive numeric variable to be mapped through the sizes of the circles.
#' @param var2 Name of the positive numeric variable to be mapped through the colors of the circles.
#' @param fixedNorm \code{FALSE} (default) : the sum of the surfaces occupied by circles is proportional to the size of the map and is declared by the \code{shareOfCircles} argument, \code{radiusMax} and \code{valueMax} are not used.\cr \code{TRUE} : the size of the largest circle is defined by a radius size (\code{radiusMax}) and a fixed variable value (\code{valueMax}), the \code{shareOfCircles} argument is not used. 
#' @param shareOfCircles Share of the surface of the map occupied by circles (0.02 is 2\%).
#' @param radiusMax Size (in cm) of the radius of the biggest circle.
#' @param valueMax Value used to normalize the size of the largest circle (in variable units).
#' @param lgdRndCircles Rounding of the values of the variable presented in the legend of the circles.
#' @param posLegCircles Position of the legend of the circles(top, bottom, left, right, center, topleft, topright, bottomleft or bottomright).
#' @param circleCol Color of the circles in the legend
#' @param baseCol Color of the base map.
#' @param nclass Number of classes to be represented.
#' @param style Method used to provide the class intervals. See \code{\link[classInt:classIntervals]{style}} in the \code{classIntervals} function from the \code{classInt} package.
#' @param fixBrks \code{FALSE} (default): the class intervals are computed through the \code{style} argument.\cr \code{TRUE} : the class intervals are provided through the \code{listBrks} argument, \code{nclass} and \code{style} are not used.
#' @param listBrks Vector of values used as breaks for the class intervals when \code{fixBrks = TRUE}.
#' @param diverg \code{FALSE} (default): there is no color break in the color palette.\cr \code{TRUE} : a break is introduced in the color palette. Palettes are defined through the \code{palColPos} and \code{palColNeg} arguments, respectively for values superior to the \code{divergBrk} value and inferior to the \code{divergBrk} value.
#' @param divergBrk Value used to define the break in the color palette if \code{diverg=TRUE}.
#' @param palCol Color palette, provided through \code{RColorBrewer}, to be used in the map. Use \code{display.brewer.all()} to see the available color ramps. For a more detailed overview : \link[RColorBrewer]{RColorBrewer}.
#' @param palColPos Palette used for values superior to \code{divergBrk} if \code{diverg=TRUE}. See \code{palCol} for details.
#' @param palColNeg Palette used for values inferior to \code{divergBrk} if \code{diverg=TRUE}. See \code{palCol} for details.
#' @param NACol Color used to draw units with no data (\code{NA}).
#' @param lgdRndDistr Rounding of the class intervals presented in the legend.
#' @param posLegDistr Position of the legend of the discretization (top, bottom, left, right, center, topleft, topright, bottomleft or bottomright).
#' @param title Title of the map.
#' @param legendCircles Title of the legend for the circles
#' @param legendDistr Title of the legend for the discretization
#' @param author	Line (author and sources) at the bottom of the map are available to display additional information. It is recommended to display the name of the author and the sources of the data and the base map.
#' @param scalebar \code{FALSE} (default): don't draw a scale bar.\cr \code{TRUE} : draws a scale bar. The choice of the scale bar location is interactive.
#' @param scalebarSize Size of the scale bar in map units.
#' @param scalebarText Text of the scale bar.
#' @param northArrow \code{FALSE} (default) : don't draw a North arrow.\cr \code{TRUE} : draws a North arrow. The choice of the North arrow location is interactive.
#' @param northArrowSize Size of the North arrow in map units.
#' @param txtCex Size of the texts.
#' @import maptools 
#' @import classInt 
#' @import RColorBrewer
#' @import sp
#' @export
#' @examples 
#' library(rCarto)
#' data(mtq)
#  minimal example
#' mtq$POPVAR <- (mtq$P09_POP-mtq$P99_POP) / mtq$P99_POP
#' mapCirclesChoro(shpFile=file.path(path.package("rCarto"), "shapes/COMMUNE"),
#'                shpId="INSEE_COM",df=mtq,dfId="ID",var="P09_POP",var2="POPVAR")
mapCirclesChoro <-
  function(shpFile,shpId,df,dfId,var,var2,
           fixedNorm=FALSE, shareOfCircles=0.02,radiusMax=0.5,
           valueMax=max(df[,var],na.rm=TRUE),
           lgdRndCircles=0,posLegCircles = "topright",
           circleCol="grey",baseCol="#FFEDA0",
           nclass=6,style="quantile",fixBrks=FALSE,listBrks=NULL,diverg=FALSE,
           divergBrk=0,
           palCol="Greens",palColPos="Reds",palColNeg="Blues",NACol="grey",
           lgdRndDistr=2,posLegDistr="bottomleft",
           title=paste(var,var2,sep=" & "),legendCircles=var,legendDistr=var2,
           author="author",
           scalebar=FALSE,scalebarSize,scalebarText,
           northArrow=FALSE,northArrowSize,
           txtCex=1){
    
    
    
    
    # import of the shapefile
    if (is.character(shpFile)){
      fdc <- maptools::readShapeSpatial(shpFile)
    }else{
      fdc <- shpFile
    }
    
    # data frame with x y coordinates of the centroids of the polygons
    pt<-cbind(fdc@data[,shpId],as.data.frame(coordinates(fdc)))
    
    # renaming of the columns' names
    colnames(pt)<-c("Code","x","y")
    
    # joint between the shapefile data and the data frame
    pt<-merge(pt,df, by.x="Code",by.y=dfId, all.x=TRUE)
    
    # suppression of zero values
    pt<-pt[pt[,var]>0,]
    
    # discretization and color management
    discretParam<-discretVar(fixBrks=fixBrks,listBrks=listBrks,pt=pt,var=var2,
                             nclass=nclass,
                             style=style,palCol=palCol,diverg=diverg,
                             divergBrk=divergBrk,palColPos=palColPos,
                             palColNeg=palColNeg,NACol=NACol,lgdRnd=lgdRndDistr)
    
    pt<-discretParam[[1]]
    colours<-discretParam[[2]]
    pdd<-discretParam[[3]]
    lblLeg<-discretParam[[4]]
    
    
    # base map display
    sp::plot(fdc, border="Grey", col=baseCol)
    pt<-circlesSize(fdc=fdc,fixedNorm=fixedNorm,pt=pt,var=var,
                    shareOfCircles=shareOfCircles,
                    radiusMax=radiusMax,valueMax=valueMax)
    title(title, sub = author)
    
    # reorder of the circles display (the largest first)
    pt<-pt[order(pt$varSize,decreasing=TRUE),]
    
    # cicles display
    symbols(pt[,c("x","y")],circles=pt$varSize,add=TRUE,bg=pt$col,inches=FALSE,
            lwd=0.5)
    
    # Distr legend display
    lgdDisplayDistr (posLeg=posLegDistr, 
                     lblLeg=lblLeg, colours=colours,  
                     na.leg=pdd , txtCexLeg=txtCex*0.6, 
                     legend=legendDistr,NACol=NACol)
    
    # Circle legend display
    lgdDisplayCircles(posLegCircles, pt=pt,varSize="varSize",var=var,
                      txtCexLeg=txtCex*0.6, legendCircles = legendCircles, 
                      circleCol=circleCol,lgdRnd = lgdRndCircles)
    
    # scalebar display
    scalebarDisplay(scalebar=scalebar,
                    scalebarSize=scalebarSize,scalebarText=scalebarText,
                    txtCexScalebar=txtCex*0.6)
    
    # north arrow display 
    northArrowDisplay(northArrow=northArrow,northArrowSize=northArrowSize)
    
    
  }
