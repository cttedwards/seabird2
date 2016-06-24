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
    
    k <- 1
    while (k < length(samples.header)) {
        if (SeaBird.is.whole.number(samples.header[k + 1])) {
            n.reps <- as.numeric(samples.header[k + 1])
            parameter.names <- c(parameter.names, paste(rep(samples.header[k], n.reps), 1:n.reps, sep = "."))
            k <- k + 2
        }
        else {
            parameter.names <- c(parameter.names, samples.header[k])
            k <- k + 1
        }
    }
    
    # extract samples from samples.1
    samples <- read.table(filename[1], skip = 1, col.names = parameter.names)
    
    # append additional sample files e.g. samples.2, samples.3, etc
    if (n.sample.files > 1) {
        if (nrow(samples) > 0) {
            samples <- data.frame(chain = 1, iter = 1:nrow(samples), samples)
        } else warning("no samples in sample file")
        for (i in 2:n.sample.files) {
            samples.chain <- read.table(filename[i], skip = 1, col.names = parameter.names)
            samples <- rbind(samples, data.frame(chain = i, iter = 1:nrow(samples.chain), samples.chain))
        }
    } else {
        if (nrow(samples) > 0) {
            samples <- data.frame(iter = 1:nrow(samples), samples)
        } else warning("no samples in sample file")
    }
    
    # return MCMC samples
    return(samples)
}
    