#' $Id: SeaBird.pos.match.R 1747 2007-11-12 21:01:38Z adunn $
#'
#' @export
#'
"SeaBird.pos.match"<-
function(vector, regexp)
{
  min((1:length(vector))[SeaBird.regexpr(regexp, vector) > 0])
}
