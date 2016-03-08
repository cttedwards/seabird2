#' set.command.with.autonumber
#'
#' @export
#'
set.command.with.autonumber = function (sbd,command,autonumber,subcommand,argument) {
	sbd[[paste(command,'[',autonumber,']',sep="")]][[subcommand]] = argument
	sbd
}

