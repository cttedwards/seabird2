#' add.command.with.label
#'
#' @export
#'
add.command.with.label = function (sbd,command,label,...) {
	item = paste(command,'[',label,']',sep="")
	sbd[[item]] = as.list(...)
	sbd[[item]]$command = command
	sbd[[item]]$value = label
	sbd
}
