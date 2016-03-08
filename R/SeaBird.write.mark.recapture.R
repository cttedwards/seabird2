#' Write mark recapture data block
#' 
#' Writes mark recapture data to a text file appropriate for insertion into \code{estimation.sbd}.
#'  
#' @param object \code{matrix} class object containing mark recapture data with labelled columns (years) and rows (band numbers).
#' @param composite_labels mark recapture data labels that correspond with composite classes
#' @param composite_map list of length equal to composite_labels with each entry containing a vector of labels assigned to each composite class
#' @param file output file name
#' @param path output file path
#' @param label label for data block
#' @param min.observations records of length <= min.observations are deleted
#' 
#' @export
#'
"SeaBird.write_mark_recapture" <- function(object, composite_labels, composite_map, file, path = './', label, min.observations = 3) {
    
    years <- colnames(object)
    rings <- rownames(object)
    
    end   <- years[length(years)]
    
    filename <- SeaBird.make.filename(path = path, file = file)
    
    firstyear <- function(x)  min(which(x != "0" & x != "-1"))
    
    rings.delete <- numeric(length(rings))
    
    cat('@mark_recapture', label, '\n', file = filename)
    cat('[insert commands here]\n', file = filename, append = TRUE)
    cat('composite_class_indices ', composite_labels, '\n', file = filename, append = TRUE)
    for (i in 1:length(composite_labels))
        cat(paste('composite_class_', composite_labels[i], ' ', paste(as.character(composite_map[[i]]), collapse = ' '), '\n', sep = ''), file = filename, append = TRUE)
    
    for (i in 1:nrow(object)) {
        
        record <- object[i,]
        record <- record[firstyear(record):length(record)]
        
        if (length(record) == 1 | (record[firstyear(record)] == 1 & length(record) <= min.observations)) {
            rings.delete[i] <- 1
            next
        }
        
        cat('banded', rings[i], file = filename, append = TRUE, sep = '_')
        cat(' ', file = filename, append = TRUE)
        cat(start[i], end, file = filename, append = TRUE)
        cat(' ', file = filename, append = TRUE)
        cat(record, '\n', file = filename, append = TRUE)
    }
    cat('\n', file = filename, append = TRUE)
    cat('banded_no', rings[!rings.delete], '\n', file = filename, append = TRUE)
    
    invisible()
}

