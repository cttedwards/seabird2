#' extract.quantities.1 
#'
#' @export
#'
extract.quantities.1 = function(lines)
{
	# lines has the following format: 
	# value
	SeaBird.string.to.vector.of.numbers(lines[1])[1]
}
