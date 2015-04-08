#' @title Choropleth
#' @name mapChoropleth
#' @description This function represents a numeric variable through a choropleth map.
#' @return A choropleth map is displayed in the graphic window.\cr
#' You can export the map in raster or vector format (pdf).
#' @details Only the five first arguments are compulsory.
#' @param shpFile Path to a polygon shapefile. 
#' @param shpId Unique identifier of the shapefile. It must correspond to \code{dfId}.
#' @param df Data frame that contains the variable to be mapped.
#' @param dfId Unique identifier of the data frame. It must correspond to \code{shpId}.
#' @param var Name of the numeric variable to be mapped.
#' @param nclass Number of classes to be represented.
#' @param style Method used to provide the class intervals. 
#' See \code{\link[classInt:classIntervals]{style}} in the \code{classIntervals} function from the \code{classInt} package.
#' @param fixBrks \code{FALSE} (default): the class intervals are computed through the \code{style} argument.\cr
#' \code{TRUE} : the class intervals are provided through the \code{listBrks} argument, \code{nclass} and \code{style} are not used.
#' @param listBrks Vector of values used as breaks for the class intervals when \code{fixBrks = TRUE}.
#' @param diverg \code{FALSE} (default): there is no color break in the color palette.\cr
#' \code{TRUE} : a break is introduced in the color palette. Palettes are defined through the \code{palColPos} and \code{palColNeg} arguments, respectively for values superior to the \code{divergBrk} value and inferior to the \code{divergBrk} value.
#' @param divergBrk Value used to define the break in the color palette if \code{diverg=TRUE}.
#' @param lgdRnd Rounding of the class intervals presented in the legend.
#' @param posLeg Position of the legend (top, bottom, left, right, center, topleft, topright, bottomleft or bottomright).
#' @param palCol Color palette, provided through \code{RColorBrewer}, to be used in the map.
#' Use \code{display.brewer.all()} to see the available color ramps. For a more detailed overview : \link[RColorBrewer]{RColorBrewer}.
#' @param palColPos Palette used for values superior to \code{divergBrk} if \code{diverg=TRUE}. See \code{palCol} for details.
#' @param palColNeg Palette used for values inferior to \code{divergBrk} if \code{diverg=TRUE}. See \code{palCol} for details.
#' @param NACol Color used to draw units with no data (\code{NA}).
#' @param title Title of the map.
#' @param legend Title of the legend.
#' @param author	Line (author and sources) at the bottom of the map are available to display additional information. It is recommended to display the name of the author and the sources of the data and the base map.
#' @param scalebar \code{FALSE} (default): don't draw a scale bar.\cr
#' \code{TRUE} : draws a scale bar. The choice of the scale bar location is interactive.
#' @param scalebarSize Size of the scale bar in map units.
#' @param scalebarText Text of the scale bar.
#' @param northArrow \code{FALSE} (default) : don't draw a North arrow.\cr
#' \code{TRUE} : draws a North arrow. The choice of the North arrow location is interactive.
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
#' mtq$POPVAR <- (mtq$P09_POP-mtq$P99_POP) / mtq$P99_POP
#' mapChoropleth(shpFile=file.path(path.package("rCarto"), "shapes/COMMUNE"),
#'               shpId="INSEE_COM",df=mtq,dfId="ID",var="POPVAR")

mapChoropleth <-
  function (shpFile,shpId,df,dfId,var,
            nclass=6,style="quantile",fixBrks=FALSE,listBrks=NULL,diverg=FALSE,
            divergBrk=0,
            lgdRnd=2,posLeg="bottomleft",
            palCol="Greens",palColPos="Reds",palColNeg="Blues",NACol="grey",
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
    
    # discretization of the variable
    discretParam<-discretVar(fixBrks=fixBrks,listBrks=listBrks,pt=fdc@data,
                             var=var,nclass=nclass,
                             style=style,palCol=palCol,diverg=diverg,
                             divergBrk=divergBrk,palColPos=palColPos,
                             palColNeg=palColNeg,NACol=NACol,lgdRnd=lgdRnd)
    
    fdc@data<-discretParam[[1]]
    colours<-discretParam[[2]]
    pdd<-discretParam[[3]]
    lblLeg<-discretParam[[4]]
    
    # map display
    plot(fdc, col=fdc$col)
    title(title, sub = author)
    # Distr legend display
    lgdDisplayDistr (posLeg=posLeg, 
                     lblLeg=lblLeg, colours=colours,  
                     na.leg=pdd , txtCexLeg=txtCex*0.6, 
                     legend=legend,NACol=NACol)
    
    # scalebar display
    scalebarDisplay(scalebar=scalebar,
                    scalebarSize=scalebarSize,scalebarText=scalebarText,
                    txtCexScalebar=txtCex*0.6)
    
    # north arrow display 
    northArrowDisplay(northArrow=northArrow,northArrowSize=northArrowSize)
    
  }
