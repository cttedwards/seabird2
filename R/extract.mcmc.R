#' extract.mcmc
#'
#' @export
#'
extract.mcmc <- function (file = "samples", path = "", n.sample.files = 1) 
{
    
    # construct filenames
    filename.preffix <- SeaBird.make.filename(path = path, file = file)
    filename         <- paste(filename.preffix, ".", 1:n.sample.files, sep = "")
    
    # extract parameter names
    samples.header  <- scan(filename[1], what = "character", nlines = 1)
    parameter.names <- c()
    
    for (i in 1:length(samples.header)) {
        
        if (i%%2 == 0) {
            parameter.names <- c(parameter.names, paste(samples.header[i - 1], "_", 1:as.integer(samples.header[i]), sep = ""))
        }
    }
    
    # extract samples from samples.1
    samples <- read.table(filename[1], skip = 1, col.names = parameter.names)
    
    # append additional sample files e.g. samples.2, samples.3, etc
    if (n.sample.files > 1) {
        
        samples <- data.frame(chain = 1, samples)
        
        for (i in 2:n.sample.files) {
            samples.chain <- read.table(filename[i], skip = 1, col.names = parameter.names)
            samples <- rbind(samples, data.frame(chain = i, samples.chain))
        }
    }
    
    # return MCMC samples
    return(samples)
}
    