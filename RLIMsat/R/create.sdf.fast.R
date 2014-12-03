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

create.sdf.fast <- function(lat.prod,lon.prod,prod,box.size=2.0,name.prod) {

    require(rgdal,quietly=TRUE)

    ## Initialize box parameters
    lim.box <- c(360-box.size,180-box.size)
    nlon <- lim.box[1]/box.size + 1; nlat <- lim.box[2]/ box.size + 1
    levels.lon <- box.size * ((-lim.box[1]/2 / box.size) : (lim.box[1]/2 / box.size))
    levels.lat <- box.size * ((-lim.box[2]/2 / box.size) : (lim.box[2]/2 / box.size))
    x <- NULL; lat <- NULL; lon <- NULL
    
    ## Create the list containing the product values for each grid point
    levels.lon.2 <- box.size * ((-180 / box.size) : (180 / box.size))
    levels.lat.2 <- box.size * ((-90 / box.size) : (90 / box.size))

    idx.lon <- as.integer(cut(lon.prod, levels.lon.2, right = FALSE))
    idx.lat <- as.integer(cut(lat.prod, levels.lat.2, right = FALSE))
    
    tmp <- vector("list",nlat); names(tmp) <- levels.lat
    for(x in names(tmp)) {tmp[[x]] <- rep(as.numeric(x),nlon)}; lat <- unlist(tmp,use.names=FALSE)
    lon <- rep(levels.lon,nlat)

    df.prod <- aggregate(prod,list(idx.lon,idx.lat),mean)
    
    x <- array(NA,nlat*nlon)
    for(i in 1:length(df.prod$x)) {
        i.lon <- df.prod$Group.1[i]; i.lat <- df.prod$Group.2[i]
        idx <- which(lon==levels.lon[i.lon] & lat==levels.lat[i.lat])
        x[idx] <- df.prod$x[[i]]
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
