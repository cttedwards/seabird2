#' extract.quantities
#'
#' @export
#'
extract.quantities = function (file, path) 
{
	quantities <- list()
	if (missing(path)) 
		path <- ""
	filename <- SeaBird.make.filename(path = path, file = file)
	file <- SeaBird.convert.to.lines(filename)
	if (all(SeaBird.regexpr("Start extracting output from here", file) < 0)) 
		stop(paste("No output data found in file '", filename, "'", sep = ""))
	file <- SeaBird.get.lines(file, clip.to = "Output quantities start here")
	file <- SeaBird.get.lines(file, clip.from = "Output quantities finish here")
	while (1) {
		if (length(file) == 0) 
			break
		header <- SeaBird.remove.first.words(SeaBird.get.lines(file, contains = "*")[1], 1)
		file <- SeaBird.get.lines(file, clip.to.match = "*")
		temp <- SeaBird.get.lines(file, clip.from.match = "*")
		if(header == "scalar parameter values") {
			quantities[[header]] = extract.quantities.scalar.parameter.values(temp)
		} else if(header == "vector parameter values") {
			quantities[[header]] = extract.quantities.vector.parameter.values(temp)
		} else if(header == "base parameter values") {
			quantities[[header]] =extract.quantities.base.parameter.values(temp)
		} else if(header == "derived parameter values") {
			quantities[[header]] = extract.quantities.derived.parameter.values(temp)
		} else if(header == "lambda") {
			quantities[[header]] = extract.quantities.lambda(temp)
		} else if(header == "initialisation_at_class") {
			quantities[[header]] = extract.quantities.initialisation_at_class(temp)
		} else if(header == "mapped initialisation_parameters") {
			quantities[[header]] = extract.quantities.mapped.initialisation_parameters(temp)
		} else if(header == "recruitment_at_class") {
			quantities[[header]] = extract.quantities.recruitment_at_class(temp)
		} else if(header == "survival_at_class") {
			quantities[[header]] = extract.quantities.survival_at_class(temp)
		} else if(header == "total_survival_at_class") {
			quantities[[header]] = extract.quantities.total_survival_at_class(temp)
		} else if(header == "mapped survival parameters") {
			quantities[[header]] = extract.quantities.mapped.survival.parameters(temp)
		} else if(header == "transition_at_class") {
			quantities[[header]] = extract.quantities.transition_at_class(temp)
		} else if(header == "mapped transition parameters") {
			quantities[[header]] = extract.quantities.mapped.transition.parameters(temp)
		} else if(header =="resight_p_at_class") {
			quantities[[header]] = extract.quantities.resight_p_at_class(temp)
		} else if(header =="mapped resight_p parameters") {
			quantities[[header]] = extract.quantities.mapped.resight_p.parameters(temp)
		} else if(header =="selectivity_at_class") {
			quantities[[header]] = extract.quantities.selectivity_at_class(temp)
		} else if(header =="mapped selectivity parameters") {
			quantities[[header]] = extract.quantities.mapped.selectivity.parameters(temp)
		} else if(header =="actual_catches") {
			quantities[[header]] = extract.quantities.actural_catches(temp)
		} else if(header =="fishing_pressures") {
			quantities[[header]] = extract.quantities.fishing_pressures(temp)
		} else if (length(temp) == 1 && length(SeaBird.string.to.vector.of.words(temp[1])) == 1) {
			quantities[[header]] <- SeaBird.string.to.vector.of.numbers(temp[1])[1]
		} else {
		    suppressWarnings(temp.try <- try(SeaBird.convert.lines.to.matrix(temp), silent = TRUE))
		    if(class(temp.try) != "try-error") {
		        quantities[[header]] <- temp.try
		    } else if(any(temp == ".X") || any(temp == ".P")) {
		        quantities[[header]] = extract.quantities.mark_recapture(temp)
		    }
		}
		if (!SeaBird.regexp.in(file, "*")) 
			break
	}
	return(quantities)
}
