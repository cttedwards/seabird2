#' extract.quantities.3
#'
#' @export
#'
extract.quantities.3 = function (lines) 
{
	# lines has the following format: 
	# no_of_items
	# item_1
	# 'year' year_1,year_2
	# value_1 value_2
	
	result <- list()
	no_of_parameters = SeaBird.string.to.vector.of.numbers(lines[1])[1]
	cursor = 2
	for (i in 1:no_of_parameters) {
		res = list()
		label <- SeaBird.string.to.vector.of.words(lines[cursor])[1]
		cursor = cursor + 1
		res[["year"]] <- SeaBird.string.to.vector.of.numbers(SeaBird.remove.first.words(lines[cursor], 1))
		cursor = cursor + 1
		res[["value"]] <- SeaBird.string.to.vector.of.numbers(lines[cursor])
		cursor = cursor + 1
		result[[label]] <- res
	}
	result
}
