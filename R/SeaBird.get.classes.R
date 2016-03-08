#' Get vector of class names from \code{population.sbd}
#'
#' @include SeaBird.convert.to.lines.R SeaBird.get.lines.R SeaBird.remove.first.words.R SeaBird.string.to.vector.of.words.R
#' @export
#'
"SeaBird.get.classes" <- function(filename) {
    
    x <- SeaBird.convert.to.lines(filename)
    x <- SeaBird.get.lines(x, starts.with = '@classes')
    x <- SeaBird.remove.first.words(x)
    x <- SeaBird.string.to.vector.of.words(x)
    
    x
}
