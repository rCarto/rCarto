#' @title Proportionnal Circles Colored by the Modalities of a Qualitative Variable
#' @name mapCirclesTypo
#' @description This function represents two variables through a proportional circles map. The areas of the circles are proportional to values in a quantitative variable and their colors reflect a the modalities of a qualitative variable.
#' @details The circles are centered on the centroids of the polygons.\cr Only the six first arguments are compulsory.
#' @return A map is displayed in the graphic window.\cr You can export the map in raster or vector format (pdf).
#' @param shpFile Path to a polygon shapefile.
#' @param shpId Unique identifier of the shapefile. It must correspond to \code{dfId}.
#' @param df Data frame that contains the variables to be mapped.
#' @param dfId Unique identifier of the data frame. It must correspond to \code{shpId}.
#' @param var Name of the positive numeric variable to be mapped through the sizes of the circles.
#' @param var2 Name of the qualitative variable to be mapped.
#' @param fixedNorm \code{FALSE} (default) : the sum of the surfaces occupied by circles is proportional to the size of the map and is declared by the \code{shareOfCircles} argument, \code{radiusMax} and \code{valueMax} are not used.\cr \code{TRUE} : the size of the largest circle is defined by a radius size (\code{radiusMax}) and a fixed variable value (\code{valueMax}), the \code{shareOfCircles} argument is not used. 
#' @param shareOfCircles Share of the surface of the map occupied by circles (0.02 is 2\%).
#' @param radiusMax Size (in cm) of the radius of the biggest circle.
#' @param valueMax Value used to normalize the size of the largest circle (in variable units).
#' @param lgdRndCircles Rounding of the values of the variable presented in the legend of the circles.
#' @param posLegCircles Position of the legend of the circles(top, bottom, left, right, center, topleft, topright, bottomleft or bottomright).
#' @param circleCol Color of the circles in the legend
#' @param baseCol Color of the base map.
#' @param posLegTypo Position of the legend of the typology (top, bottom, left, right, center, topleft, topright, bottomleft or bottomright).
#' @param palCol Color palette, provided through \code{RColorBrewer}, to be used in the map. Use \code{display.brewer.all()} to see the available color ramps. For a more detailed overview : \link[RColorBrewer]{RColorBrewer}.
#' @param NACol Color used to draw units with no data (\code{NA}).
#' @param title Title of the map.
#' @param legendCircles Title of the legend for the circles
#' @param legendTypo Title of the legend for the typology
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
#' # minimal example
#' mtq[c(1,12,18,23,33,8,24),"beach"] <- "No access to the beach"
#' mtq[c(2,4,5,6,9,13,17,20,21,25,26,29,31,34,11,27,7,19),"beach"] <- "Caribbean Sea"
#' mtq[c(14,15,16,22,28,30,32,10,3),"beach"] <- "Atlantic Ocean"
#' mapCirclesTypo(shpFile=file.path(path.package("rCarto"), "shapes/COMMUNE"),
#'                shpId="INSEE_COM",df=mtq,dfId="ID",var="P09_POP",var2="beach")
#' 
mapCirclesTypo <-
  function(shpFile,shpId,df,dfId,var,var2,
           fixedNorm=FALSE, shareOfCircles=0.02,radiusMax=0.5,
           valueMax=max(df[,var],na.rm=TRUE),
           lgdRndCircles=0,posLegCircles = "topright",
           circleCol="grey",baseCol="#FFEDA0",
           posLegTypo="bottomleft",
           palCol="Paired",NACol="grey",
           title=paste(var,var2,sep=" & "),legendCircles=var,legendTypo=var2,
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
    # typo of the variable
    typoParam<-typo(pt=pt,var=var2, palCol=palCol,NACol=NACol)
    pt<-typoParam[[1]]
    lblLeg<-typoParam[[2]]
    pdd<-typoParam[[3]]
    
    # base map display
    sp::plot(fdc, border="Grey", col=baseCol)
    title(title, sub = author)
    pt<-circlesSize(fdc=fdc,fixedNorm=fixedNorm,pt=pt,var=var,
                    shareOfCircles=shareOfCircles,
                    radiusMax=radiusMax,valueMax=valueMax)
    
    # reorder of the circles display (the largest first)
    pt<-pt[order(pt$varSize,decreasing=TRUE),]
    
    # cicles display
    symbols(pt[,c("x","y")],circles=pt$varSize,add=TRUE,bg=pt$varQuali,
            inches=FALSE,lwd=0.5)
    
    # legend display
    lgdDisplayQualit(posLeg=posLegTypo, lblLeg=lblLeg,  
                     na.leg=pdd , txtCexLeg=txtCex*0.6, 
                     NACol=NACol, legend=legendTypo) 
    
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
