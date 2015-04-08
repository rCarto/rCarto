#' @title Typology
#' @name mapChoroTypo
#' @description This function represents a qualitative variable through a map. Polygons are filled in relation to the variable modalities.
#' @param shpFile Path to a polygon shapefile. 
#' @param shpId Unique identifier of the shapefile. It must correspond to \code{dfId}.
#' @param df Data frame that contains the variable to be mapped.
#' @param dfId Unique identifier of the data frame. It must correspond to \code{shpId}.
#' @param var Name of the qualitative variable to be mapped.
#' @param posLeg Position of the legend (top, bottom, left, right, center, topleft, topright, bottomleft or bottomright).
#' @param palCol Color palette, provided through \code{RColorBrewer}, to be used in the map. Use \code{display.brewer.all()} to see the available color ramps. For a more detailed overview : \link[RColorBrewer]{RColorBrewer}.
#' @param NACol Color used to draw units with no data (\code{NA}).
#' @param title Title of the map.
#' @param legend Title of the legend.
#' @param author	Line (author and sources) at the bottom of the map are available to display additional information. It is recommended to display the name of the author and the sources of the data and the base map.
#' @param scalebar \code{FALSE} (default): don't draw a scale bar.\cr \code{TRUE} : draws a scale bar. The choice of the scale bar location is interactive.
#' @param scalebarSize Size of the scale bar in map units.
#' @param scalebarText Text of the scale bar.
#' @param northArrow \code{FALSE} (default) : don't draw a North arrow.\cr \code{TRUE} : draws a North arrow. The choice of the North arrow location is interactive.
#' @param northArrowSize Size of the North arrow in map units.
#' @param txtCex Size of the texts.
#' @details Only the five first arguments are compulsory.
#' @return A typology map is displayed in the graphic window.\cr You can export the map in raster or vector format (pdf).
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
#' mapChoroTypo(shpFile=file.path(path.package("rCarto"), "shapes/COMMUNE"),
#'              shpId="INSEE_COM",df=mtq,dfId="ID",var="beach")  
mapChoroTypo <-
  function (shpFile,shpId,df,dfId,var,
            posLeg="bottomleft",
            palCol="Paired",NACol="grey",
            title=var,legend=var,author="author",
            scalebar=FALSE,scalebarSize,scalebarText,
            northArrow=FALSE,northArrowSize,
            txtCex=1){
    

    # import of the shapefile
    if (is.character(shpFile)){
      fdc <- maptools::readShapeSpatial(shpFile)
    }else{
      fdc <- shpFile
    }
    
    # joint between the shapefile data and the data frame
    fdc@data <- data.frame(fdc@data, df[match(fdc@data[,shpId], df[,dfId]),])
    
    # typo of the variable
    typoParam<-typo(pt=fdc@data,var=var, palCol=palCol,NACol=NACol)
    fdc@data<-typoParam[[1]]
    lblLeg<-typoParam[[2]]
    pdd<-typoParam[[3]]
    
   
    # map display
    sp::plot(fdc, col=fdc@data$varQuali)
    title(title, sub = author)
    
    # legend display
    lgdDisplayQualit(posLeg=posLeg, lblLeg=lblLeg,  
                     na.leg=pdd , txtCexLeg=txtCex*0.6, 
                     NACol=NACol, legend=legend) 
    
    # scalebar display
    scalebarDisplay(scalebar=scalebar,
                    scalebarSize=scalebarSize,scalebarText=scalebarText,
                    txtCexScalebar=txtCex*0.6)
    
    # north arrow display 
    northArrowDisplay(northArrow=northArrow,northArrowSize=northArrowSize)
    
}
