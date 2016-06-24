#' Extract projection results to data frame
#'
#' @export
#'
"extract.projection" <- function(file = "", path = "", write.to.file = TRUE) {
    
    filename <- SeaBird.make.filename(path = path, file = file)
    
    filelines <- SeaBird.convert.to.lines(filename)
    filelines <- SeaBird.get.lines(filelines, clip.to = "Quantity values :")
    
    filelines <- SeaBird.trim.lines(filelines)
    
    filename <- paste0(path, '/projection.out')
    
    cat(filelines, file = filename, sep = "\n", append = FALSE)
    
    parameter.names   <- scan(filename[1], what = "character", nlines = 1)
    parameter.samples <- read.table(filename, skip = 1, col.names = parameter.names)
    
    if (!write.to.file) 
        file.remove(filename)
    
    return(parameter.samples)
}
