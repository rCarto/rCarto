#' @title Proportional Circles
#' @name mapCircles
#' @details The circles are centered on the centroids of the polygons.
#' Only the five first arguments are compulsory.
#' @return A proportionnal circle map is displayed in the graphic window.
#' You can export the map in raster or vector format (pdf).
#' @param shpFile	Path to a polygon shapefile.
#' @param shpId	Unique identifier of the shapefile. It must correspond to dfId.
#' @param df	Data frame that contains the variable to be mapped.
#' @param dfId	Unique identifier of the data frame. It must correspond to shpId.
#' @param var	Name of the positive numeric variable to be mapped.
#' @param fixedNorm	FALSE (default) : the sum of the surfaces occupied by circles is proportional to the size of the map and is declared by the shareOfCircles argument, radiusMax and valueMax are not used.
#' TRUE : the size of the largest circle is defined by a radius size (radiusMax) and a fixed variable value (valueMax), the shareOfCircles argument is not used.
#' @param shareOfCircles	Share of the surface of the map occupied by circles (0.02 is 20 percent).
#' @param radiusMax	Size (in cm) of the radius of the biggest circle.
#' @param valueMax	Value used to normalize the size of the largest circle (in variable units).
#' @param lgdRnd	Rounding of the values of the variable presented in the legend.
#' @param posLeg	Position of the legend (top, bottom, left, right, center, topleft, topright, bottomleft or bottomright).
#' @param circleCol	Color of the circles.
#' @param baseCol	Color of the base map.
#' @param title	Title of the map.
#' @param legend	Title of the legend.
#' @param author	Two lines (author and sources) at the bottom of the map are available to display additional information. It is recommended to display the name of the author and the sources of the data and the base map.
#' @param sources	Two lines (author and sources) at the bottom of the map are available to display additional information. It is recommended to display the name of the author and the sources of the data and the base map.
#' @param scalebar	FALSE (default): don't draw a scale bar.
#' TRUE : draws a scale bar. The choice of the scale bar location is interactive.
#' @param scalebarSize	Size of the scale bar in map units.
#' @param scalebarText Text of the scale bar.
#' @param northArrow	FALSE (default) : don't draw a North arrow.
#' TRUE : draws a North arrow. The choice of the North arrow location is interactive.
#' @param northArrowSize	Size of the North arrow in map units.
#' @param txtCex	Size of the texts.
#' @import maptools 
#' @import classInt 
#' @import RColorBrewer
#' @import sp
#' @export
#' @examples 
#' # library(rCarto)
#' data(mtq)
#' # minimal example
#' mapCircles(shpFile=file.path(path.package("rCarto"), "shapes/COMMUNE"),
#'            shpId="INSEE_COM",df=mtq,dfId="ID",var="P09_POP", )
#' 
#' # detailed example
#' mapCircles(shpFile=file.path(path.package("rCarto"), "shapes/COMMUNE"),
#'            shpId="INSEE_COM",df=mtq,dfId="ID",var="P09_POP",
#'            shareOfCircle=0.1,
#'            lgdRnd=0,circleCol="Red",
#'            title="Population distribution in Martinique",
#'            legend="Total resident\npopulation in 2009",
#'            author=Sys.getenv("USERNAME"),
#'            sources="data : INSEE,2009; basemap : IGN, 2012")
#'            
mapCircles <-
  function (shpFile,shpId,df,dfId,var,
            fixedNorm=FALSE, shareOfCircles=0.02, radiusMax=0.5,
            valueMax=max(df[,var],na.rm=TRUE),
            lgdRnd=0, posLeg="bottomleft",
            circleCol="#FD8D3C", baseCol="#FFEDA0",
            title=var, legend=var, author="author", sources="sources",
            scalebar=FALSE, scalebarSize, scalebarText,
            northArrow=FALSE,northArrowSize,
            txtCex=1){
    

    # import of the shapefile
    if (is.character(shpFile)){
      fdc <- maptools::readShapeSpatial(shpFile)
    }else{
      fdc <- shpFile
    }
    
    # data frame with x y coordinates of the centroids of the polygons
    pt <- cbind(fdc@data[,shpId],as.data.frame(sp::coordinates(fdc)))
    
    # renaming of the columns' names
    colnames(pt) <- c("Code","x","y")
    
    # joint between the shapefile data and the data frame
    pt <- merge(pt,df, by.x="Code",by.y=dfId, all.x=TRUE)
    
    # suppression of zero values
    pt <- pt[pt[,var] > 0, ]
    
    # base map display
    sp::plot(fdc, border="Grey", col=baseCol)
    title(title, sub = author)
    
    # Size of the circles
    pt<-circlesSize(fdc=fdc,fixedNorm=fixedNorm,pt=pt,var=var,
                    shareOfCircles=shareOfCircles,
                    radiusMax=radiusMax,valueMax=valueMax)
    
    # reorder of the circles display (the largest first)
    pt <- pt[order(pt$varSize,decreasing=TRUE),]
    
    # cicles display
    symbols(pt[,c("x","y")],circles=pt$varSize,add=TRUE,bg=circleCol,
            inches=FALSE,lwd=0.5)
    
    # legend display
    lgdDisplayCircles(posLegCircles=posLeg, pt=pt,varSize="varSize",var=var,
                      txtCexLeg=txtCex*0.6, legendCircles = legend , 
                      circleCol=circleCol,lgdRnd = lgdRnd)
    # 
    # # scalebar display
    scalebarDisplay(scalebar=scalebar,
                    scalebarSize=scalebarSize,scalebarText=scalebarText,
                    txtCexScalebar=txtCex*0.6)

    # # north arrow display 
    northArrowDisplay(northArrow=northArrow,northArrowSize=northArrowSize)
    

  }
