#' extract.quantities.6
#' 
#' @export
#'
extract.quantities.6 <- function(lines)
{
    result <- array(dim = c(length(lines), length(SeaBird.string.to.vector.of.numbers(lines[1]))))
	for (i in 1:length(lines))
	    result[i,] <- SeaBird.string.to.vector.of.numbers(lines[i])
	rownames(result) <- names(lines)
	
	result
}
