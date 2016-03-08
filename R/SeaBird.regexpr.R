#' Regular expression matching
#'
#' @export
#'
"SeaBird.regexpr" <- function(x, y, fixed = T)
{

    return(regexpr(x, y, fixed=fixed))

}
