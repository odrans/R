#' Grid an array of latlon dataset.
#'
#' This function returns a list of gridded dataset.
#' @param lon Array; Longitudes of the dataset.
#' @param lat Array; Latitudes of the dataset.
#' @param z Array; Dataset to be represented in the spatial dataframe.
#' @param box.size Numeric; Size of the gridbox where the data is averaged. Default is set to 2.0 degrees.
#' @keywords grid
#' @export
#' @examples
#' To come.

grid.list <- function(lon, lat, z, box.size = 2) {
    lon <- replace(lon,lon==180,179.99)
    
    levels.lon <- box.size * ((-180 / box.size) : (180 / box.size))
    levels.lat <- box.size * ((-90 / box.size) : (90 / box.size))

    idx.lon <- as.integer(cut(lon, levels.lon, right = FALSE))
    idx.lat <- as.integer(cut(lat, levels.lat, right = FALSE))

    tbl <- array(list(NA), c(360 / box.size + 1, 180 / box.size + 1))
    for (i in 1 : length(z)) {
        tbl[[idx.lon[i], idx.lat[i]]] <- c(tbl[[idx.lon[i], idx.lat[i]]], z[i])
    }
    tbl
}
