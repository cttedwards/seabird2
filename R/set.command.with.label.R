#' set.command.with.label
#'
#' @export
#'
set.command.with.label = function (sbd,command,label,subcommand,argument) {
	sbd[[paste(command,'[',label,']',sep="")]][[subcommand]] = argument
	sbd
}
