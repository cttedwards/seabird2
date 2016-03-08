#' extract.quantities.4
#'
#' @export
#'
extract.quantities.4 = function(lines)
{
	# lines has the following format: 
	# no_of_items
	# item_1
	# no_of_years
	# 'year' year_1
	# value
	
	result <- list()
	no_of_items = SeaBird.string.to.vector.of.numbers(lines[1])[1]
	cursor = 2
	for (i in 1:no_of_items) {
		res_item = list()
		label <- SeaBird.string.to.vector.of.words(lines[cursor])[1]
		cursor = cursor + 1
		no_of_years = SeaBird.string.to.vector.of.numbers(lines[cursor])[1]
		cursor = cursor + 1
		for (j in 1:no_of_years) {
			year = SeaBird.string.to.vector.of.words(SeaBird.remove.first.words(lines[cursor], 1))
			cursor = cursor + 1 
			value = SeaBird.string.to.vector.of.numbers(lines[cursor])
			cursor = cursor + 1 
			res_item [[year]] = value
		}		
		result[[label]] <- res_item
	}
	result
}
