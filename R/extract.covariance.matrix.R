#' $Id: extract.covariance.matrix.R 1828 2007-11-30 01:16:07Z adunn $
#'
#' @export
#'
"extract.covariance.matrix"<-
function(file, path="")
{
  if(missing(path)) path<-""
  filename<-SeaBird.make.filename(path=path,file=file)
  lines<-SeaBird.convert.to.lines(filename)
  if(all(SeaBird.regexpr("Covariance matrix",lines)<0)) stop("No covariance matrix was found. Did you set @print.covariance=True in your output.csl file?")
  lines<-SeaBird.get.lines(lines,clip.to.match="Covariance matrix")
  columns <- SeaBird.string.to.vector.of.words(lines[1])
  if(length(lines) < 2) return(NA)
  data <- matrix(0, length(columns), length(columns))
  for(i in 1:nrow(data))
    data[i,] <- SeaBird.string.to.vector.of.numbers(lines[i])
  data<-data[1:ncol(data),]
  return(data)
}
