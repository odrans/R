#' List the content of a hdf4 file
#'
#' This function returns SDS and Vdata contained in a hdf4 file.
#' @param fname A character providing the name of the hdf4 file.
#' @param ignore.vd A logical flag indicating if the Vdata are ignored or not (default TRUE)
#' @keywords hdf4
#' @export
#' @examples
#' fname <- data.hdf
#' list.sds <- h4list(fname,ignore.vd=FALSE)  ## List the SDS and Vdata contained in data.hdf

h4list <- function(fname, ignore.vd=TRUE) {

    options(warn=-1)

    if(!file.exists(fname)) return("This file doesn't exists")
    
    ##-------------------------------------------------------------------------------------------------------------
    isds <- 0; sds.name <- NULL; attr(sds.name,'status')=NULL
    l.sds <- matrix(NA,nrow=500,ncol=2,dimnames=list(array("SDS",500),array(c("Index","Name"),2)))
    while(is.null(attr(sds.name,'status'))) {
        sds.name <- system(paste('hdp dumpsds -h -i ',isds,' ',fname,' | grep Variable',sep=''),intern=TRUE)

        if(is.null(attr(sds.name,'status'))) {
            l.sds[isds+1,2] <- strsplit(sds.name,"= ")[[1]][2]; l.sds[isds+1,1] <- isds
        }
        
        isds <- isds + 1
    }
    nsds <- isds - 1
    l.sds <- l.sds[1:nsds,1:2]
    ##-------------------------------------------------------------------------------------------------------------

    
    if(!ignore.vd) {
        ##-------------------------------------------------------------------------------------------------------------
        ivd <- 1; vd.name <- "ok"
        while(!grepl("Values",vd.name)) {
            vd.name <- system(paste('hdp dumpvd -h -i ',ivd,' ',fname,' | grep fields',sep=''),intern=TRUE)
            ivd <- ivd + 1
        }
        nvd <- ivd - 2
        ##-------------------------------------------------------------------------------------------------------------
        
        ##-------------------------------------------------------------------------------------------------------------
        l.vd <- matrix(NA,nrow=nvd,ncol=2,dimnames=list(array("Vdata",nvd),array(c("Index","Name"),2)))
        if(nvd > 0) {
            for(ivd in 1:nvd) {
                vd.name <- system(paste('hdp dumpvd -h -i ',ivd,' ',fname,' | grep fields',sep=''),intern=TRUE)
                l.vd[ivd,2] <- strsplit(vd.name,"= ")[[1]][2]; l.vd[ivd,1] <- ivd
            l.vd[ivd,2] <- substring(l.vd[ivd,2],2,nchar(l.vd[ivd,2])-2)
            }
        }
        ##-------------------------------------------------------------------------------------------------------------
    }

    if(ignore.vd) {
        return(rbind(l.sds))
    } else {
        return(rbind(l.sds,l.vd))
    }

    options(warn=0)
}
