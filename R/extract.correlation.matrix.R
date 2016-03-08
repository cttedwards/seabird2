#' extract.correlation.matrix
#'
#' @export
#'
"extract.correlation.matrix"<-
function(file, path="")
{
  filename<-SeaBird.make.filename(path=path,file=file)
  lines<-SeaBird.convert.to.lines(filename)
  if(all(SeaBird.regexpr("Correlation matrix",lines)<0)) stop("No correlation matrix was found. Did you set @print.covariance=True in your output.csl file?")
  lines<-SeaBird.get.lines(lines,clip.to.match="Correlation matrix")
  columns <- SeaBird.string.to.vector.of.words(lines[1])
  if(length(lines) < 2) return(NA)
  data <- matrix(0, length(columns), length(columns))
  for(i in 1:nrow(data))
    data[i,] <- SeaBird.string.to.vector.of.numbers(lines[i])
  data<-data[1:ncol(data),]
  return(data)
}
