#' $Id: SeaBird.unpaste.R 1747 2007-11-12 21:01:38Z adunn $
#'
#' @export
#'
"SeaBird.unpaste"<-
function(string, sep)
{
    unlist(strsplit(string, sep))
}
