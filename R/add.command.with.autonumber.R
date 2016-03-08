#' add.command.with.autonumber
#'
#' @export
#'
add.command.with.autonumber = function (sbd,command,autonumber,...) {
	item = paste(command,'[',autonumber,']',sep="")
	sbd[[item]] = as.list(...)
	sbd[[item]]$command = command
	sbd[[item]]$value = character(0)
	sbd
}
