#' Copy a file
#'
#' This function is an extension of file.copy that can also copy to/from on a remote host.
#' @param from.to character vectors, containing file names or paths.
#' @param overwrite logical; should existing destination files be overwritten? (not yet implemented for remote host)
#' @param recursive logical.  If 'to' is a directory, should directories in 'from' be copied (and their contents)? (not yet implemented for remote host)
#' @param copy.mode logical: should file permission bits be copied where possible? (not yet implemented for remote host)
#' @keywords copy, remote
#' @export
#' @examples
#' ## On remote host,
#' lf("host:~/user/data/data.hdf","~/user/data/")

rcp <- function(from, to, overwrite = FALSE, recursive = FALSE, copy.mode = TRUE) {

    if(grepl(":",from)) {
        options(warn=-1)
        
        cmd <- paste("rsync -av ",from," ",to,sep="")
        system(cmd)
        
        options(warn=0)
    } else {
        
        file.copy(from, to, overwrite = overwrite, recursive = recursive,
                  copy.mode = copy.mode, copy.date = copy.date)
    }
    
}
