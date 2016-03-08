#' set.command.with.nolabel
#'
#' @export
#'
set.command.with.nolabel = function (sbd,command,subcommand,argument) {
	sbd[[command]][[subcommand]] = argument
	sbd
}
