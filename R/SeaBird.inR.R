#' $Id: SeaBird.inR.R 1747 2007-11-12 21:01:38Z adunn $
#'
#' @export
#'
"SeaBird.inR"<-
function()
{
  if(!exists("version")) return(F)
  else if(is.null(version$language)) return(F)
  else return(version$language == "R")
}
