#' extract.header 
#'
#' @export
#'
extract.header = function (file, path) 
{
	header <- list()
	if (missing(path)) 
		path <- ""
	filename <- SeaBird.make.filename(path = path, file = file)
	file <- SeaBird.convert.to.lines(filename)
	header$call <- SeaBird.remove.first.words(SeaBird.get.lines(file, 
					starts.with = "Call:"), 1)
	header$date <- SeaBird.remove.first.words(SeaBird.get.lines(file, 
					starts.with = "Date:"), 1)
	header$version <- SeaBird.get.lines(file, starts.with = "v")[1]
	header$user <- SeaBird.remove.first.words(SeaBird.get.lines(file, 
					starts.with = "User name:"), 2)
	header$machine <- SeaBird.remove.first.words(SeaBird.get.lines(file, 
					starts.with = "Machine name:"), 2)
	return(header)
}
