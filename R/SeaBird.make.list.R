#' $Id: SeaBird.make.list.R 1747 2007-11-12 21:01:38Z adunn $
#'
#' @export
#'
"SeaBird.make.list"<-
function(lines)
{
  result <- list()
  for(i in 1:length(lines)) {
    label <- SeaBird.string.to.vector.of.words(lines[i])[1]
    contents <- SeaBird.string.to.vector.of.numbers(SeaBird.remove.first.words(lines[i],1))
    result[[label]] <- contents
  }
  result
}
