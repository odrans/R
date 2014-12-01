#' Plot a projected map of a given dataset
#'
#' This function returns plots of a mapped given dataset. The initial dataset is averagged into a grid before being resampled, interpolated and projected.
#' @param Lat Array; Latitudes of the dataset.
#' @param Lon Array; Longitudes of the dataset.
#' @param Prod Array; Dataset to be represented in the spatial dataframe.
#' @param proj Character; Name of the output projection (correspond to the names of rgdal PROJ4 projections, call projInfo for more information). Current supported projections are: aeqd, cass, eck1-2-3-4-5-6, eqc, gall, goode, igh, longlat, mill, moll, poly, robin, siny, vandg, wag1-2-3-4-5-6. Default is set to eck4.
#' @param sampling Character; Type of resampling (near, bilinear, cubic, cubicspline, lanczos, average, mode). Default is set to cubic.
#' @param dir.Gtiff Character; Directory where is stored the raster dataset in a GTiff format. Default is ".".
#' @param interp Logical; Activate the interpolation of the dataset during resampling. Default is TRUE.
#' @param overwrite Logical; If FALSE, the function tries to re-use an existing GTiff file, otherwise a new one is created. Default is TRUE.
#' @param box.size Numeric; Size of the gridbox where the data is averaged. Default is set to 2.0 degrees.
#' @param color Array/Character; The input can be a color palette or the name of a ColorBrewer palette. Default is the Spectral type of ColorBrewer.
#' @param ncol Numeric; Number of colors in the output colorbors. The input palette is interpolated.
#' @param ... Additional arguments passed to image.plot (fields package).
#' @keywords projection, mapping
#' @export
#' @examples
#' To come.

map.plot <- function(Lat,Lon,Prod,proj="eck4",sampling="cubic",interp=TRUE,overwrite=TRUE,dir.GTiff=".",color="",ncol=64,box.size=2,postprocess=function(x){return(x)},...) {

    require(rworldmap,quietly=TRUE)
    require(fields,quietly=TRUE)

    name.prod <- deparse(substitute(Prod))
    name.prod <- gsub("$",".",name.prod,fixed=TRUE)
    name.prod <- gsub("(","",name.prod,fixed=TRUE); name.prod <- gsub(")","",name.prod,fixed=TRUE)

    if(length(color)==1) {
        require(RColorBrewer)
        if(color=="") {
            color <- rev(colorRampPalette(brewer.pal(11,"Spectral"), space = "rgb")(ncol))
        } else {
            color <- rev(colorRampPalette(brewer.pal(11,col), space = "rgb")(ncol))
        }
    } else {
        color <- colorRampPalette(col, space = "rgb")(ncol)
    }

    tif.proj <- paste(paste(dir.GTiff,"GTiff",sep="/"),"/",name.prod,"-",proj,"-",sampling,".tif",sep="")
    if(!file.exists(tif.proj)&!overwrite) {
        warning("Corresponding GTiff file not existing, overwrite is set to TRUE",immediate.=TRUE)
        overwrite <- TRUE
    }


    if(overwrite) sdf.latlon <- create.sdf(Lat,Lon,Prod,box.size,name.prod)

    sdf.proj <- project.sdf(sdf.latlon,proj,sampling,dir.GTiff,interp,overwrite,name.prod)
    map.proj <- project.map(proj)

    sdf.proj[[name.prod]] <- postprocess(sdf.proj[[name.prod]])
    sdf.proj[[name.prod]] <- replace(sdf.proj[[name.prod]],is.nan(sdf.proj[[name.prod]]),NA)

    sdf.proj[[name.prod]] <- replace(sdf.proj[[name.prod]],is.nan(sdf.proj[[name.prod]]),NA)

    options <- list(...)
    idx.zlim <- which(names(options)=="zlim"); zlim <- options[[idx.zlim]]

    sdf.proj[[name.prod]] <- replace(sdf.proj[[name.prod]],!is.na(sdf.proj[[name.prod]]) && sdf.proj[[name.prod]]<zlim[1],zlim[1])
    sdf.proj[[name.prod]] <- replace(sdf.proj[[name.prod]],!is.na(sdf.proj[[name.prod]]) && sdf.proj[[name.prod]]>zlim[2],zlim[2])
    
    image.plot(sdf.proj,nlevel=ncol,col=color,...)
    mapCountryData(map.proj,numCats=1,addLegend=FALSE,colourPalette = c("grey88","grey88"),add=T,borderCol = "black",mapTitle="")
}



