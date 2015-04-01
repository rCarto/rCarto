\name{commune}
\alias{commune}
\title{
Shapefile of Communes of the Martinique Island
}
\description{
Shapefile of communes of the Martinique island (France). This shapefile is a GEOFLA extract.
}
\format{
  A shapefile with 34 observations on 18 variables.
  \describe{
    \item{\code{ID_GEOFLA}}{identifier of the polygons}
    \item{\code{CODE_COMM}}{other identifier of the polygons}
    \item{\code{INSEE_COM}}{identifier of the polygons to be used in examples}
    \item{\code{NOM_COMM}}{names of the communes}
    \item{\code{...}}{not used}
  }
}
\source{
http://professionnels.ign.fr/geofla#tab-3

Original data can be retreived here (page reached on 2012-11-19) : \cr
http://professionnels.ign.fr/sites/default/files/GEOFLA_1-1_SHP_UTM20W84_MQ-ED111.tar.gz
}

\examples{
## to read the shapefile in R
library(rCarto)
shpMtq<-maptools::readShapeSpatial(file.path(path.package("rCarto"), "shapes/COMMUNE"))
}
