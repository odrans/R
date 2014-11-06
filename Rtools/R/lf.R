#' List the files in current local machine or on a remote host.
#'
#' This function is an extension of list.files that can also list the files on a remote computer.
#' @param path a character vector of full path names; the default corresponds to the working directory, 'getwd()'.  Tilde expansion (see 'path.expand') is performed.  Missing values will be ignored. ":" is used to indicate a link to a remote host. The name of the host preceeds the semi-column.
#' @param pattern an optional regular expression.  Only file names which match the regular expression will be returned.
#' @param all.files a logical value.  If 'FALSE', only the names of visible files are returned.  If 'TRUE', all file names will be returned.
#' @param full.names a logical value.  If 'TRUE', the directory path is prepended to the file names to give a relative file path.  If 'FALSE', the file names (rather than paths) are returned.
#' @param recursive logical.  Should the listing recurse into directories? (not yet implemented for remote host)
#' @param ignore.case logical.  Should pattern-matching be case-insensitive? (not yet implemented for remote host)
#' @param include.dirs logical.  Should subdirectory names be included in recursive listings?  (They always are in non-recursive ones).
#' @param no.. logical.  Should both '"."' and '".."' be excluded also from non-recursive listings? (automatic for remote host)
#' @keywords list, remote
#' @export
#' @examples
#' ## On local machine, similarly to list.files
#' lf("~/user/data")
#' ## On remote host,
#' lf("host:~/user/data")

lf <- function(path = ".", pattern = NULL, all.files = FALSE,
               full.names = FALSE, recursive = FALSE,
               ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE) {
    
    if(grepl(":",path)) {
        if(length(strsplit(path,":")[[1]])>2) {stop('Multiple ":" in the path.')}
        
        options(warn=-1)
        
        host <- strsplit(path,":")[[1]][1]
        path.host <- strsplit(path,":")[[1]][2]
        
        cmd <- paste("ssh -t ",host," -t ",sep="")
        cmd.ls <- paste(cmd,"ls -1 ",path.host,sep="")
        cmd.lsa <- paste(cmd,"ls -1a ",path.host,sep="")

        if(!all.files) {
            ls <- system(cmd.ls,ignore.stderr=TRUE,intern=TRUE)
        } else {
            ls <- system(cmd.lsa,ignore.stderr=TRUE,intern=TRUE)
            filter <- which(!(ls==".." | (ls==".")))
            ls <- ls[filter]
        }
        
        if(!is.null(pattern)) {
            filter <- which(grepl(pattern,ls))
            ls <- ls[filter]
        }

        if(full.names) {
            ls <- sapply(ls, function(x) {if(path.host!="/") {paste(path,x,sep="/")} else {paste(path,x,sep="")}},USE.NAMES=FALSE)
        }

        options(warn=0)
        
        return(ls)
        
    } else {
        list.files(path, pattern, all.files, full.names, recursive,
                   ignore.case, include.dirs, no..)
    }
}
