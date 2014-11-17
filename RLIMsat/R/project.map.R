#' Project a spatial data frame
#'
#' This function returns a projected map
#' @param proj Character; Name of the output projection (correspond to the names of rgdal PROJ4 projections, call projInfo for more information). Current supported projections are: aeqd, cass, eck1-2-3-4-5-6, eqc, gall, goode, igh, longlat, mill, moll, poly, robin, siny, vandg, wag1-2-3-4-5-6. Default is set to eck4.
#' @keywords projection, map
#' @export
#' @examples
#' To come.

project.map <- function(proj="eck4") {
    
    require(rworldmap,quietly=TRUE)
    require(raster,quietly=TRUE)

    ## Load the latlon map from rworldmap
    map.latlon <- getMap()
    options(warn=-1); proj4string(map.latlon) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"); options(warn=0)

    ## Project the map
    map.proj <- spTransform(map.latlon,CRS(paste("+proj=",proj,sep="")))

    return(map.proj)
    
}
