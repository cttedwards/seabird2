#' Run SeaBird from within an R terminal session
#'
#' Currently only runs an MPD fit using \code{SeaBird -e}. The session output is piped to a file called \code{mpd.out}.
#'
#' @param exe.dir location of SeaBird.exe
#' @param sbd.dir location of input files
#' @param results.dir location in which to save mpd output
#' @param external logical indicating whether an external command window should be opened
#' @export
#'
"SeaBird" <- function(exe.dir = '.', sbd.dir = exe.dir, results.dir = exe.dir, external = FALSE) {
    
    cmd <- paste(exe.dir,'SeaBird -e -f ',sbd.dir,' > ',results.dir,'mpd.out',sep = '')
    
    message(paste('running SeaBird from:', sbd.dir, '; with mpd.out saved in:', results.dir))
    shell(cmd, translate = TRUE, wait = !external, invisible = !external, mustWork = TRUE)
    
}
