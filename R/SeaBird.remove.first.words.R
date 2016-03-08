#' $Id: SeaBird.remove.first.words.R 1747 2007-11-12 21:01:38Z adunn $
#'
#' @export
#'
"SeaBird.remove.first.words"<-
function(string, words = 1)
{
  paste(SeaBird.unpaste(string, sep = " ")[ - (1:words)], collapse = " ")
}
