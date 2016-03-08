#' Write transition matrix to text file
#' 
#' Two files are produced from the transition matrix input, called \code{transition_block.sbd} and \code{classes_block.sbd}. These should be pasted into \code{population.sbd}.
#' 
#' @param object \code{tmatrix} class object
#' @export
# generic function
"write_blocks" <- function(x, ...) UseMethod("write_blocks")
#' @rdname write_blocks
#' @export
# method
"write_blocks.tmatrix" <- function(object, path = '.') {
    
    filename <- SeaBird.make.filename(path = path, file = '/transition_block.sbd')
    
    cat('@transition\n', file = filename)
    
    parameter.map   <- as.vector(object$transition)
    parameter.names <- object$par.names
    
    cat('\tparameter_map'  , parameter.map,   '\n',  file = filename, append = TRUE)
    cat('\tparameter_names', parameter.names, '\n',  file = filename, append = TRUE)
    
    filename <- SeaBird.make.filename(path = path, file = '/classes_block.sbd')
    
    cat('@classes', object$names, file = filename)
    
    invisible()
}
