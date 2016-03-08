#' extract.quantities.transition_at_class
#'
#' @export
#'
extract.quantities.transition_at_class= function (lines) 
{
	result = extract.quantities.4(lines)
	for (i in 1:length(result[[1]])) {
		res = result[[1]][[i]]
		res = matrix(res,nrow=sqrt(length(res)),ncol=sqrt(length(res)),byrow=T)
		result[[1]][[i]] = res
	}
	result
}	
