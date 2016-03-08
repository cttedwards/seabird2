#' extract.quantities.resight_p_at_class
#'
#' @export
#'
extract.quantities.resight_p_at_class <- function (lines) 
{
	result.temp <- extract.quantities.4(lines)
	
	labels <- names(result.temp)
	
	result <- vector('list', length(labels))
	names(result) <- labels
	
	for (i in 1:length(result)) {
	    result[[i]] <- plyr::laply(result.temp[[i]], function(x) x)
	    if (is.matrix(result[[i]])) 
	        rownames(result[[i]]) <- names(result.temp[[i]])
	}
	
	result
}
