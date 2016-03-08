#' $Id: SeaBird.get.lines.R 1747 2007-11-12 21:01:38Z adunn $
#'
#' @export
#'
"SeaBird.get.lines" <- function(lines, from = -1, to = -1, contains = "", starts.with = "", clip.to = "", clip.from = "", clip.to.match = "", clip.from.match = "", ...)
{
  result <- lines
  if(from > 0) {
    result <- result[(1:length(result)) >= from]
  }
  if(to > 0) {
    if(from > 0) to <- to - from + 1
    result <- result[(1:length(result)) <= to]
  }
  if(clip.to != "") {
    result <- result[(SeaBird.pos(result, clip.to) + 1):length(result)]
  }
  if(clip.from != "") {
    result <- result[1:(SeaBird.pos(result, clip.from) - 1)]
  }
  if(clip.to.match != "") {
    if(SeaBird.regexp.in(result, clip.to.match)) {
      result <- result[(SeaBird.pos.match(result, clip.to.match) + 1):length(result)]
    }
  }
  if(clip.from.match != "") {
    if(SeaBird.regexp.in(result, clip.from.match)) {
      result <- result[1:(SeaBird.pos.match(result, clip.from.match) - 1)]
    }
  }
  if(contains != "") {
    result <- result[SeaBird.regexpr(contains, result) > 0]
  }
  if(starts.with != "") {
    result <- result[SeaBird.regexpr(starts.with, result) > 0]
  }
  return(result)
}
