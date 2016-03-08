#' $Id: extract.quantities.from.table.R 1828 2007-11-30 01:16:07Z adunn $
#'
#' @export
#'
"extract.quantities.from.table"<-
function(file, path = "")
{
  filename<-SeaBird.make.filename(path=path,file=file)
  lines <- SeaBird.convert.to.lines(filename)
  lines <- SeaBird.get.lines(lines, clip.to.match = "Quantity values")
  quantities <- SeaBird.make.table(lines)
  return(quantities)
}
