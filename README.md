# rCarto
*01-04-2015*  
***This repo releases the rCarto v0.9.***

## Major changes
* Use of <code>layout</code>: The previous version of rCarto (v0.8, the one on the CRAN) used a specific (and potentialy problematic) way of displaying title, sources, author and map togethers. This solution was good to create maps with perfectly fitted width and height (in cm). But due to the use of the <code>layout</code> function it was not possible to edit the plotted map (adding labels, points or anything elese)
The current version does not use this solution anymore. 
* Import of Spatial-DataFrame: The only way to use a basemap in the previous version was via a shapefile. In this version it is possible to use an object of class sp (SpatialPolygonsDataFrame for example)
* Use of in-code documentation via roxygen2: it makes the code easier to understand
* Only 2 functions are available (mapCircles and mapChoropleth)  


## Future Developments
I will not put this version on the CRAN as it is a major update that may probably break scripts using the previous version.  
Except by the addition of the other functions of the package (mapChoroTypo etc.),  I will probably not develop the package further.  

So, fork me if you want more features!
