#' $Id: SeaBird.make.sbd.commands.R 1801 2007-11-22 21:15:33Z adunn $
#'
#' @export
#'
"SeaBird.make.sbd.commands"<-
function(file="dictionary.cpp", path)
{
  set.class <- function(object, new.class) {
    # use in the form
    # object <- set.class(object,"new class")
    attributes(object)$class <- c(new.class, attributes(object)$class[attributes(object)$class != new.class])
    object
  }
  if(missing(path)) path=""
  if(path != "" & substring(path, nchar(path) - 1) != "\\") path <- paste(path, "\\", sep = "")
  # create filename and read in file
  file<-"dictionary.cpp"; path <- "c:\\cygwin\\home\\fud\\SEABIRD\\src\\"
  filename <- paste(path, file, sep = "")
  res <- SeaBird.convert.to.lines(filename)
  res<-SeaBird.get.lines(res, clip.to.match = "//Start extracting sbd.commands", clip.from.match = "//End extracting sbd.commands")
  res <- res[substring(res, 1, 11) == "set_command"]
  type<-substring(res,18,SeaBird.regexpr("(",res)-1)
  command<-substring(res,SeaBird.regexpr("(",res)+1,SeaBird.regexpr(",",res)-1)
  ans<-data.frame("command"=as.character(command),"type"=as.character(type),stringsAsFactors=FALSE)
  return(ans)
}
#sbd.commands<-SeaBird.make.sbd.commands(file="dictionary.cpp",path="c:\\cygwin\\home\\fud\\SeaBird\\src")
#cat("# $Id: SeaBird.make.sbd.commands.R 1801 2007-11-22 21:15:33Z fud $\n",file="c:\\cygwin\\home\\fud\\seabird\\R-libraries\\SeaBird\\R\\SeaBird.commands.R")
#dump("sbd.commands", file="c:\\cygwin\\home\\fud\\seabird\\R-libraries\\SeaBird\\R\\sbd.commands.R",append=T,control=NULL)
