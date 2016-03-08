#' extract.objective.function
#'
#' @export
#'
extract.objective.function = function (file, path = "") 
{
	objective <- list()
	if (missing(path)) 
		path <- ""
	filename <- SeaBird.make.filename(path = path, file = file)
	file <- SeaBird.convert.to.lines(filename)
	if (all(SeaBird.regexpr("Start extracting output from here", 
					file) < 0)) 
		stop(paste("No output data found in file '", filename, 
						"'", sep = ""))
	file <- SeaBird.get.lines(file, clip.to = "Start extracting output from here")
	objective$value <- as.numeric(SeaBird.remove.first.words(SeaBird.get.lines(file, starts.with = "Objective function :")[1], 3))
	file <- SeaBird.get.lines(file, clip.to.match = "Components :", 
			clip.from.match = "Fits :")
	n.components <- length(file)
	objective$components <- data.frame(matrix(0, n.components, 
					2))
	names(objective$components) <- c("label", "value")
	for (i in 1:n.components) {
		objective$components[i, 1] <- SeaBird.string.to.vector.of.words(file[i])[1]
		objective$components[i, 2] <- as.numeric(SeaBird.string.to.vector.of.words(file[i])[2])
	}
	return(objective)
}
