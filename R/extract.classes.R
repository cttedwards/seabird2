#' extract.classes
#'
#' @export
#'
"extract.classes" <- function(file, path = "./")
{
  
  filename <- SeaBird.make.filename(path = path, file = file)
  
  lines <- SeaBird.convert.to.lines(filename)
  line  <- SeaBird.get.lines(lines, starts.with = '@classes') 
  
  classes <- SeaBird.string.to.vector.of.words(line)[-1]
  
  return(classes)
}
