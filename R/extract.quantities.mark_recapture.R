#' extract.quantities.mark_recapture
#'
#' @export
#'
extract.quantities.mark_recapture <- function(temp)
{
    is.X <- any(temp == ".X")
    is.P <- any(temp == ".P")
    
    result <- list()
    
    if (is.X)
    {
        temp.X <- temp
        temp.X <- SeaBird.get.lines(temp.X, clip.to.match   = ".X")
        if (is.P) {
            if (match(".X", temp) < match(".P", temp))
                temp.X <- SeaBird.get.lines(temp.X, clip.from.match = ".P")
        }
        
        # array headers
        header.lines <- which(vapply(temp.X, function(x) !(length(SeaBird.string.to.vector.of.numbers(x)) > 1), logical(1)))
        header.names <- names(header.lines)
        
        # extract array for each header
        temp.X.list <- list()
        for (i in 1:(length(header.lines) - 1))
        {
            temp.X.list[[header.names[i]]] <- SeaBird.convert.lines.to.matrix(temp.X, from = header.lines[i] + 1, to = header.lines[i + 1] - 1)
        }
        temp.X.list[[header.names[length(header.lines)]]] <- SeaBird.convert.lines.to.matrix(temp.X, from = header.lines[length(header.lines)] + 1, to = length(temp.X))
        
        result[['X']] <- temp.X.list
    }
    
    if (is.P)
    {
        temp.P <- temp
        temp.P <- SeaBird.get.lines(temp.P, clip.to.match = ".P")
        if (is.X) {
            if (match(".P", temp) < match(".X", temp))
                temp.P <- SeaBird.get.lines(temp.P, clip.from.match = ".X")
        }
        
        
        # array headers
        header.lines <- which(vapply(temp.P, function(x) !(length(SeaBird.string.to.vector.of.numbers(x)) > 1), logical(1)))
        header.names <- names(header.lines)
        
        # extract array for each header
        temp.P.list <- list()
        for (i in 1:(length(header.lines) - 1))
        {
            temp.P.list[[header.names[i]]] <- SeaBird.convert.lines.to.matrix(temp.P, from = header.lines[i] + 1, to = header.lines[i + 1] - 1, row.names = TRUE)
        }
        temp.P.list[[header.names[length(header.lines)]]] <- SeaBird.convert.lines.to.matrix(temp.P, from = header.lines[length(header.lines)] + 1, to = length(temp.P), row.names = TRUE)
        
        result[['P']] <- temp.P.list
    }
    
    result
    
}