#' extract.fits
#'
#' @export
#'
extract.fits = function (file, path = "") {
	fits <- list()
	if (missing(path)) 
		path <- ""
	filename <- SeaBird.make.filename(path = path, file = file)
	file <- SeaBird.convert.to.lines(filename)
	if (all(SeaBird.regexpr("Start extracting output from here", file) < 0)) 
		stop(paste("No output data found in file '", filename, "'", sep = ""))
	file <- SeaBird.get.lines(file, clip.to = "Start extracting output from here")
	file <- SeaBird.get.lines(file, clip.to = "The observations are:")
	observations <- SeaBird.get.lines(file, clip.from = "Fits follow:")
	file <- SeaBird.get.lines(file, clip.to = "Fits follow:")
	for (i in 1:length(observations)) {
		this.observation <- observations[i]
		fits[[this.observation]] <- list()
		file <- SeaBird.get.lines(file, clip.to.match = this.observation)
		abundance <- SeaBird.regexp.in(file[1], "abundance")
                Bycatches <- SeaBird.regexp.in(file[1], "Bycatches")
		mark.recapture <- SeaBird.regexp.in(file[1], "mark_recapture")
		if (i < length(observations)) {
			if (!mark.recapture) 
				temp <- SeaBird.get.lines(file, from = 2, clip.from.match = observations[i + 1])
			else temp <- SeaBird.get.lines(file, from = 2, clip.from.match = "objective function:")
		}
		else {
			temp <- SeaBird.get.lines(file, from = 2, to = -1)
			if (!abundance  && !mark.recapture) {
				temp2 <- SeaBird.get.lines(temp, clip.to.match = "obs", clip.from.match = "fits")
				n.lines <- nrow(SeaBird.make.table(temp2))
				temp <- SeaBird.get.lines(file, from = 2, to = seabird.pos.match(file, "objective function:") + n.lines - 1)
			}
		}
		if (abundance || Bycatches) {
			temp2 <- SeaBird.get.lines(temp, from = 1, clip.from.match = "objective function:")
			temp3 <- SeaBird.get.lines(temp, clip.to.match = "objective function:")
			data <- SeaBird.make.table(temp2)
			fits[[this.observation]]$year <- data$year
			fits[[this.observation]]$obs <- data$obs
			fits[[this.observation]]$fits <- data$fits
			if (SeaBird.regexp.in(names(data), "resids")) {
				fits[[this.observation]]$resids <- data$resids
			}
			if (SeaBird.regexp.in(names(data), "Pearson_resids")) {
				fits[[this.observation]]$pearson.resids <- data[["Pearson_resids"]]
			}
			if (SeaBird.regexp.in(names(data), "Normalised_resids")) {
				fits[[this.observation]]$normalised.resids <- data[["Normalised_resids"]]
			}
			if (SeaBird.regexp.in(names(data), "Normalised_resids")) {
				fits[[this.observation]]$normalised.resids <- data[["Normalised_resids"]]
			}
			fits[[this.observation]]$error.value <- SeaBird.string.to.vector.of.numbers(temp3[1:length(data$year)])
		} else if(!abundance &&!Bycatches && !mark.recapture) {
                          temp2 <- SeaBird.get.lines(temp,clip.to.match="obs",clip.from.match="fits")
                          columns<-temp2[1]
                          obs.table <- SeaBird.make.table(temp2)
                          fits[[this.observation]]$year <- obs.table[,1]
                          fits[[this.observation]]$obs <- data.frame(obs.table[,-1])
                          row.names(fits[[this.observation]]$obs) <- obs.table[,1]
                          if(SeaBird.regexp.in(temp, "resids"))
                              temp2 <- SeaBird.get.lines(temp, clip.to.match = "fits", clip.from.match = "resids")
                          else
                              temp2 <- SeaBird.get.lines(temp, clip.to.match = "fits", clip.from.match = "objective")
                          fits.table <- SeaBird.make.table(temp2)
                          fits[[this.observation]]$fits <- data.frame(fits.table[,-1])
                          row.names(fits[[this.observation]]$fits) <- obs.table[,1]
                          if(SeaBird.regexp.in(temp,"^resids")){
                              temp2 <- SeaBird.get.lines(temp,clip.to.match="^resids",clip.from.match="resids")
                              if(length(temp2)!=nrow(fits[[this.observation]]$obs))
                              temp2 <- temp2[1:(nrow(fits[[this.observation]]$obs)+1)]
                              resids.table <- SeaBird.make.table(temp2)
                              fits[[this.observation]]$resids <- data.frame(resids.table[,-1])
                              row.names(fits[[this.observation]]$resids) <- resids.table[,1]
                          }
                          if(SeaBird.regexp.in(temp,"pearson_resids")){
                              if(SeaBird.regexp.in(temp,"normalised_resids"))
                                  temp2 <- SeaBird.get.lines(temp,clip.to.match="pearson_resids",clip.from.match="resids")
                              else
                                  temp2 <- SeaBird.get.lines(temp,clip.to.match="pearson_resids",clip.from.match="objective")
                              pearson.resids.table <- SeaBird.make.table(temp2)
                              fits[[this.observation]]$pearson.resids <- data.frame(pearson.resids.table[,-1])
                              row.names(fits[[this.observation]]$pearson.resids) <- pearson.resids.table[,1]
                          }
                          if(SeaBird.regexp.in(temp,"normalised_resids")){
                              temp2 <- SeaBird.get.lines(temp,clip.to.match="normalised_resids",clip.from.match="objective")
                              normalised.resids.table <- SeaBird.make.table(temp2)
                              fits[[this.observation]]$normalised.resids <- data.frame(normalised.resids.table[,-1])
                              row.names(fits[[this.observation]]$normalised.resids) <- normalised.resids.table[,1]
                          }
                          if(SeaBird.regexp.in(temp,"objective")){
                              temp2 <- SeaBird.get.lines(temp,clip.to.match="objective")
                              if(!SeaBird.regexp.in(temp,"user-supplied likelihood")){
                                  temp2 <- temp2[1:(nrow(fits[[this.observation]]$obs))]
                                  columns <- paste(SeaBird.string.to.vector.of.words(columns)[-1],collapse = " ")
                                  error.value.table <- SeaBird.make.table(c(columns,temp2))
                                  fits[[this.observation]]$error.value <-  data.frame(error.value.table)
                                  row.names(fits[[this.observation]]$error.value) <- obs.table[, 1]
                              }
                          }
                }
	}
	return(fits)
}
