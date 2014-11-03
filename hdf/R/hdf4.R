#' Read hdf4 files
#'
#' This function returns the content of a hdf4 file as a list.
#' @param fname Name of the hdf4 file.
#' @param sds.name Array containing all the SDS or vdata contained in the hdf4 file (provided by h4list).
#' @param sds Name of the SDS to extract.
#' @keywords hdf4
#' @export
#' @examples
#' h4read(CALIOP.hdf,sds.CALIOP,"Latitude")
#'
#' 
## .
## fname: / sds: sds name (if it works) or number (0 included)
##================================================================================================================
h4read <- function(fname,sds.name,sds,...) {
    library(rgdal,quietly=TRUE)
    
    idx.sds.name <- which(sds.name[,2]==sds)
    idx.sds <- sds.name[idx.sds.name,1]; type.sds <- attr(idx.sds.name,"names")
    
    if(grepl("SDS",type.sds)) {
        sds.name <- paste('HDF4_SDS:UNKNOWN:"%s":',idx.sds,sep="")
        
        sds <- getRasterData(GDAL.open(sprintf(sds.name,fname)),...)

        sds.info <- GDALinfo(sprintf(sds.name,fname),returnStats=FALSE)
        attr(sds,"mdata") <- attr(sds.info,"mdata")
    } else if(grepl("Vdata",type.sds)) {
        sds <- system(paste('hdp dumpvd -d -i ',idx.sds,' ',fname,sep=''),intern=TRUE)
        sds <- as.numeric(sds[!is.na(as.numeric(sds))])
    }
        
    return(sds)
}
##================================================================================================================
