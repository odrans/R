#' Transfrom an array of data into a gridded spatial data frame
#'
#' This function transforms an array of latlon data into a gridded latlon data frame. The data is averaged in a gridbox.
#' @param lat.prod Array; Latitudes of the dataset.
#' @param lon.prod Array; Longitudes of the dataset.
#' @param prod Array; Dataset to be represented in the spatial dataframe.
#' @param box.size Numeric; Size of the gridbox where the data is averaged. Default is set to 2.0 degrees.
#' @param name.prod Character; Name of the data
#' @keywords grid, map
#' @export
#' @examples
#' To come.

create.sdf <- function(lat.prod,lon.prod,prod,box.size=2.0,name.prod) {

    suppressMessages(suppressWarnings(require(rgdal)))

    ## Initialize box parameters
    lim.box <- c(360-box.size,180-box.size)
    nlon <- lim.box[1]/box.size + 1; nlat <- lim.box[2]/ box.size + 1
    levels.lon <- box.size * ((-lim.box[1]/2 / box.size) : (lim.box[1]/2 / box.size))
    levels.lat <- box.size * ((-lim.box[2]/2 / box.size) : (lim.box[2]/2 / box.size))
    x <- NULL; lat <- NULL; lon <- NULL
    
    ## Create the list containing the product values for each grid point
    l <- grid.list(lon.prod,lat.prod,prod,box.size)    
    
    ## Prepare the vectors for the sdf
    for(ilat in 1:nlat) {
        for(ilon in 1:nlon) {
            if(length(l[[ilon,ilat]])>1) {
                x <- c(x,mean(l[[ilon,ilat]],na.rm=TRUE))
            } else {
                x <- c(x,l[[ilon,ilat]])
            }
            lon <- c(lon,levels.lon[ilon])
            lat <- c(lat,levels.lat[ilat])
        }
    }
    x <- replace(x,x<=0 | is.na(x),0.0)

    ## Create the sdf
    z <- data.frame(lon,lat,x)
    coordinates(z) <- ~lon+lat
    proj4string(z) <- CRS("+proj=longlat")
    gridded(z) <- TRUE
    z <- as(z, "SpatialGridDataFrame")

    names(z) <- name.prod
    
    return(z)
    
}
