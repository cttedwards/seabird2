#' $Id: SeaBird.remove.last.words.R 1747 2007-11-12 21:01:38Z adunn $
#'
#' @export
#'
"SeaBird.remove.last.words"<-
function(string, words = 1)
{
  temp <- SeaBird.unpaste(string, sep = " ")
  to.drop <- length(temp) - (0:(words - 1))
  paste(unlist(temp[ - to.drop]), collapse = " ")
}
