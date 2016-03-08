#' extract.aic
#'
#' @export
#'
extract.aic <- function (file, path = "") 
{
    negative.log.likelihood <- extract.objective.function(file, path)[['value']]
    free.pars               <- extract.free.parameters(file, path)
    
    n.pars <- length(unlist(free.pars))
    
    aic <- 2 * n.pars + 2 * negative.log.likelihood 
    
    return(list(n.pars = n.pars, negative.log.likelihood = negative.log.likelihood, aic = aic))
}
    