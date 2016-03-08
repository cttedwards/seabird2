#' extract.quantities.5
#'
#' @export
#'
extract.quantities.5 = function(lines)
{
	# lines has the following format: 
	# no_of_items
	# item_1
	# no_of_years
	# 'year' year_1
	# no_of_steps
	# 'step' step_1
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
			res_year = list()
			year = SeaBird.string.to.vector.of.words(SeaBird.remove.first.words(lines[cursor], 1))
			cursor = cursor + 1 
			no_of_steps = SeaBird.string.to.vector.of.numbers(lines[cursor])[1]
			cursor = cursor + 1
			for(k in 1:no_of_steps) {
				step = SeaBird.string.to.vector.of.words(SeaBird.remove.first.words(lines[cursor], 1))
				cursor = cursor + 1 
				value = SeaBird.string.to.vector.of.numbers(lines[cursor])
				cursor = cursor + 1 
				res_year [[step]] = value
			}
			res_item [[year]] = res_year
		}		
		result[[label]] <- res_item
	}
	result
}
