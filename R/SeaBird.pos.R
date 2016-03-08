#' $Id: SeaBird.pos.R 1747 2007-11-12 21:01:38Z adunn $
#'
#' @export
#'
"SeaBird.pos"<-
function(vector, x)
{
  min((1:length(vector))[vector == x])
}
