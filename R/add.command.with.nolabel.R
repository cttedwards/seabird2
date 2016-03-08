#' add.command.with.nolabel
#'
#' @export
#'
add.command.with.nolabel = function (sbd,command,...) {
	sbd[[command]] = as.list(...)
	sbd[[command]]$command = command
	sbd[[command]]$value = character(0)
	sbd
}
