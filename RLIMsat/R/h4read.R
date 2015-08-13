#' Read hdf4 files
#'
#' This function returns the content of a hdf4 file as a list for a given SDS.
#' @param fname A character containing the name of the hdf4 file.
#' @param sds.name An array containing all the SDS or vdata contained in the hdf4 file (provided by h4list).
#' @param sds A character indicating the SDS to extract.
#' @keywords hdf4
#' @export
#' @examples
#' fname <- data.hdf
#' list.sds <- h4list(fname,ignore.vd=FALSE)  ## List the SDS and Vdata contained in data.hdf
#' value.sds <- h4read(fname,list.sds,"Latitude") ## extract "Latitude" from data.hdf

h4read <- function(fname,sds.name,sds,...) {

    suppressMessages(suppressWarnings(require(rgdal)))

    if(!file.exists(fname)) {
        warning("This file doesn't exists",immediate.=TRUE)
        return(NULL)
    }
    
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
