#' Transition matrix class
#' 
#' @export
setClass("tmatrix", contains = "list")#, representation(trannames = "character", par.names = "character"))
setMethod("initialize", "tmatrix", function(.Object, names) {
    
    .Object@.Data <- list(transition = array(0, dim = c(length(names), length(names)), dimnames = list(to = names, from = names)), names = names, par.names = character()) 
    
    return(.Object)
})