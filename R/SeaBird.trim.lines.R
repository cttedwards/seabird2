#' Trim rows so that any unfinished iterations are removed
#'
#' @export
#'
"SeaBird.trim.lines" <- function(lines) {
    
    ff <- function(x) { 
        y <- SeaBird.string.to.vector.of.words(x) 
        return(length(y)) 
    }
    
    line.lengths <- unlist(lapply(lines, ff))
    
    if (any(line.lengths < line.lengths[1]))
        lines <- lines[-which(line.lengths < line.lengths[1])]
    
    return(lines)
    
}
