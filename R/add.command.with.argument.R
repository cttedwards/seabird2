#' add.command.with.argument
#'
#' @export
#'
add.command.with.argument = function (sbd,command,argument) {
	sbd[[command]] = list()
	sbd[[command]]$command = command
	sbd[[command]]$value = argument
}
