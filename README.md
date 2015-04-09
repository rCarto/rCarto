# rCarto
***Release of rCarto v0.9***

----------------
*2015-04-09*   

En français [ici](http://rgeomatic.hypotheses.org/529)

## Major changes   
* Use of <code>layout</code> - The previous version of rCarto (v0.8, the one on the CRAN) used a specific (and potentialy problematic) way of displaying title, source, author and map together. This solution was good for creating maps with perfectly fitted width and height (in cm). But due to the use of the <code>layout</code> function it was not possible to edit the plotted map (adding labels, points or anything else)
The current version does not use this solution anymore. 
* Use of Spatial*DataFrame - The only way to use a basemap in the previous version was via shapefiles. In this version it is possible to use an object of class sp (SpatialPolygonsDataFrame for example) as basemap
* Use of "in-code"" documentation via roxygen2 - Roxygen2 make the documentation easier to edit and to read while developping

## Future Developments
I will not put this version on the CRAN as it is a major update that may break scripts using the previous version.  
I will probably not develop the package further.  

So, fork me if you want more features!

## Install Instructions
<code><pre>require(devtools)  
devtools::install_github(repo = 'rCarto/rCarto')  
</pre></code>
