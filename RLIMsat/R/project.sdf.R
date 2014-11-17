#' Project a spatial data frame
#'
#' This function projects a longlat dataframe into a given projection. The data can also be resampled and interpolated.
#' @param sds.latlon Spatial gridded dataframe; Input data in longlat projection.
#' @param proj Character; Name of the output projection (correspond to the names of rgdal PROJ4 projections, call projInfo for more information). Current supported projections are: aeqd, cass, eck1-2-3-4-5-6, eqc, gall, goode, igh, longlat, mill, moll, poly, robin, siny, vandg, wag1-2-3-4-5-6. Default is set to eck4.
#' @param sampling Character; Type of resampling (near, bilinear, cubic, cubicspline, lanczos, average, mode). Default is set to cubic.
#' @param dir.Gtiff Character; Directory where is stored the raster dataset in a GTiff format. Default is ".".
#' @param interp Logical; Activate the interpolation of the dataset during resampling. Default is TRUE.
#' @param overwrite Logical; If FALSE, the function tries to re-use an existing GTiff file, otherwise a new one is created. Default is TRUE.
#' @param name.prod Character; Name of the data
#' @keywords  projection
#' @export
#' @examples
#' To come.

project.sdf <- function(sdf.latlon,proj="eck4",sampling="cubic",dir.GTiff=".",interp=TRUE,overwrite=TRUE,name.prod="") {

    require(raster,quietly=TRUE)

    if(name.prod=="") name.prod <- names(sdf.latlon)
    if(interp&proj=="longlat") {
        warning("Current interpolation process cannot deal with the longlat to longlat projections. interp is set to FALSE.",immediate.=TRUE)
        interp=FALSE
    }
    
    ## Set the GTiff files
    dir <- paste(dir.GTiff,"GTiff",sep="/"); if(!file.exists(dir)) dir.create(dir)
    tif.latlon <- paste(dir,paste(name.prod,"-longlat.tif",sep=""),sep="/")
    tif.proj <- paste(dir,paste(name.prod,"-",proj,"-",sampling,".tif",sep=""),sep="/")
    if(overwrite) {
        if(file.exists(tif.latlon)) file.remove(tif.latlon)
        if(file.exists(tif.proj)) file.remove(tif.proj)
    }

    ## Transform the data frame into a raster
    r.latlon <- raster(sdf.latlon)

    ## Create the latlon raster file
    if(!file.exists(tif.latlon) | overwrite) tmp <- writeRaster(r.latlon,tif.latlon, drivername = "GTiff",overwrite=TRUE)
    
    ## Create the projected raster file
    if(!file.exists(tif.proj) | overwrite) {
        cmd.sampling <- c(paste(" -r ", sampling,sep=" "))
        cmd.proj <- c(paste(" -t_srs '+proj=",proj,"'",sep=""))
        if(interp) {cmd.interp <- c(" -tr  6000 3000")} else {cmd.interp <- ""}
        options(warn=-1)
        status <- system(paste("gdalwarp",cmd.sampling,cmd.interp,cmd.proj," ",tif.latlon," ",tif.proj,sep=""),ignore.stdout=TRUE,ignore.stderr=TRUE,inter=TRUE)
        options(warn=1)
        if(!is.null(attr(status,"status"))) {
            stop(paste("The warping from longlat to",proj,"has failed, please select another projection."))
        }

    } else {
        warning(paste("A projection to ",proj," with the ",sampling," sampling already exists for ",name.prod,
                      " and will be re-used. Set overwrite to TRUE to reprocess the file.",sep=""),immediate.=TRUE)
    }

    ## Read the projected raster file
    r.proj <- raster(tif.proj)
    sdf.proj <- as(r.proj,"SpatialGridDataFrame")
    names(sdf.proj) <- name.prod
    
    sdf.proj[[names(sdf.proj)]] <- replace(sdf.proj[[names(sdf.proj)]] ,sdf.proj[[names(sdf.proj)]] ==16200 | sdf.proj[[names(sdf.proj)]] <=1,NA)
    
    return(sdf.proj)

}
