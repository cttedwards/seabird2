#' Transition matrix constructor function
#' 
#' @export
# generic function
"populate" <- function(x, ...) UseMethod("populate")
#' @rdname populate
#' @export
# method
"populate.tmatrix" <- function(object, from, to, name, verbose = TRUE) {
    
    counter <- max(object$transition) + 1
    
    if (length(from) != length(to)) 
        warning('from and to should be same length')
    
    for (i in 1:length(to))
        object$transition[to[i], from[i]] <- counter
    
    if (missing(name))
        name <- paste('T', object$names[from], object$names[to], sep = '')
    
    object$par.names[counter] <- name
    
    if (verbose) 
        print(object)
    
    return(object)
}
