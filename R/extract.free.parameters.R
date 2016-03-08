#' extract.free.parameters
#'
#' @export
#'
extract.free.parameters <- function(file,path="",type=c("values","bounds","both"))
		{
		parameters <- list()
		bounds <- list()
		valid.types<-c("values","bounds","both")
		type<-valid.types[pmatch(type[1],valid.types)]
		if(!(type %in% valid.types)) stop("Argument type is invalid. It should be one of 'values', 'bounds', or 'both'")
		if(missing(path)) path<-""
		filename<-SeaBird.make.filename(path=path,file=file)
		file <- SeaBird.convert.to.lines(filename)
		if(all(SeaBird.regexpr("Start extracting output from here",file)<0)) stop(paste("No output data found in file '",filename,"'",sep=""))
		file <- SeaBird.get.lines(file, clip.to = "Start extracting output from here")
		# Look for sets of 2 consecutive lines of the form
		# scalar_parameter_name
		# current value: scalar_parameter_value
		# lower bounds:
		# upper bounds:
		current.value.incidences <- (1:length(file))[SeaBird.regexpr("current value:", file) > 0]
		if(length(current.value.incidences) > 0) {
			for(i in 1:length(current.value.incidences)) {
				scalar.parameter.name <- file[current.value.incidences[i] - 1]
				if(SeaBird.regexpr("lower bound",file[current.value.incidences[i]+1])>0) {
					scalar.parameter.value <-  SeaBird.string.to.vector.of.numbers(SeaBird.remove.first.words(file[current.value.incidences[i]], 2))
					parameters[[scalar.parameter.name]] <- scalar.parameter.value
					scalar.lower.bound.value <-  SeaBird.string.to.vector.of.numbers(SeaBird.remove.first.words(file[current.value.incidences[i]+1], 2))
					scalar.upper.bound.value <-  SeaBird.string.to.vector.of.numbers(SeaBird.remove.first.words(file[current.value.incidences[i]+2], 2))
					bounds[[scalar.parameter.name]] <- list("lower.bound"=scalar.lower.bound.value,"upper.bound"=scalar.upper.bound.value)
				} else {
					scalar.parameter.value <-  SeaBird.string.to.vector.of.numbers(SeaBird.remove.first.words(file[current.value.incidences[i]], 2))
					parameters[[scalar.parameter.name]] <- scalar.parameter.value
					bounds[[scalar.parameter.name]] <- list("lower.bound"=NA,"upper.bound"=NA)
				}
			}
		}
		# Look for sets of consecutive lines of the form
		# vector_parameter_name
		# current values: vector_parameter_value
		# lower bounds:
		# upper bounds:
		# or (if casal version < 2.10-2007/12/05
		# vector_parameter_name
		# current values:
		# vector_parameter_value
		current.values.incidences <- (1:length(file))[SeaBird.regexpr("current values:", file) > 0]
		if(length(current.values.incidences) > 0) {
			for(i in 1:length(current.values.incidences)) {
				vector.parameter.name <- file[current.values.incidences[i] - 1]
				if(SeaBird.regexp.in(vector.parameter.name, "parameters")) {
					vector.parameter.name <- SeaBird.remove.last.words(vector.parameter.name, 1)
				}
				if(SeaBird.regexpr("lower bound",file[current.values.incidences[i]+1])>0) {
					vector.parameter.value <- SeaBird.string.to.vector.of.numbers(SeaBird.remove.first.words(file[current.values.incidences[i]],2))
					parameters[[vector.parameter.name]] <- vector.parameter.value
					vector.lower.bound.value <-  SeaBird.string.to.vector.of.numbers(SeaBird.remove.first.words(file[current.values.incidences[i]+1], 2))
					vector.upper.bound.value <-  SeaBird.string.to.vector.of.numbers(SeaBird.remove.first.words(file[current.values.incidences[i]+2], 2))
					bounds[[vector.parameter.name]] <- list("lower.bound"=vector.lower.bound.value,"upper.bound"=vector.upper.bound.value)
				} else {
					vector.parameter.value <-  SeaBird.string.to.vector.of.numbers(SeaBird.remove.first.words(file[current.values.incidences[i]], 2))
					parameters[[vector.parameter.name]] <- vector.parameter.value
					bounds[[vector.parameter.name]] <- list("lower.bound"=rep(NA,length(vector.parameter.value)),"upper.bound"=rep(NA,length(vector.parameter.value)))
				}
			}	
		}
		if(type=="values") return(parameters)
		else if(type=="bounds") return(bounds)
		else if(type=="both") return(list("values"=parameters,"bounds"=bounds))
		else return(NA)
}
		
