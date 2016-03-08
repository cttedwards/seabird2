#' $Id: SeaBird.regexp.in.R 1747 2007-11-12 21:01:38Z adunn $
#'
#' @export
#'
"SeaBird.regexp.in"<-
function(vector, regexp)
{
  if(length(vector) == 0)
    return(F)
  any(SeaBird.regexpr(regexp, vector) > 0)
}
