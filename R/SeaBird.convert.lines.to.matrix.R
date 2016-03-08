#' SeaBird.convert.lines.to.matrix
#'
#' @export
#'
"SeaBird.convert.lines.to.matrix" <- function(lines, from = -1, to = -1, contains = "", starts.with = "", clip.to = "", clip.from = "", clip.to.match = "", clip.from.match = "", row.names = FALSE, ...)
{
    result <- SeaBird.get.lines(lines, from, to, contains, starts.with, clip.to, clip.from, clip.to.match, clip.from.match)
    
    result.row.names <- character(length(result))
    
    if (row.names)
        for (i in 1:length(result)) {
            result.row.names[i] <- SeaBird.string.to.vector.of.words(result[i])[1]
            result[i]           <- SeaBird.remove.first.words(result[i])
        }

    result <- extract.quantities.6(result)
    
    if (row.names)
        rownames(result) <- result.row.names
    
    result
}
