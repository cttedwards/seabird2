#' Get initial state from output file
#'
#' This requires that \code{initial_state} is set to \code{T} in the \code{output.sbd} file.
#'
#' @export
"extract.initial.state" <- function(file, path) {
    
    if (missing(path)) 
        path <- ""
    filename <- SeaBird.make.filename(path = path, file = file)
    file <- SeaBird.convert.to.lines(filename)
    file <- file[file != "Dumping Taylor tape to disk - see BB"]
    if (all(SeaBird.regexpr("Initial state:", file) < 0)) 
        stop(paste("No initial state found in file '", filename, "'", sep = ""))
    loc  <- max(which(SeaBird.regexpr("Initial state:", file) > 0))
    file <- SeaBird.get.lines(file, from = loc + 2, to = loc + 3)
    
    res        <- SeaBird.string.to.vector.of.numbers(file[2])
    names(res) <- SeaBird.string.to.vector.of.words(file[1])
    
    return(res)
}
