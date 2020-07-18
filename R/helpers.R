#' @export
last_call <- function(max.lookback=100L){
  # from https://github.com/mrdwab/overflow-mrdwab/blob/master/R/solast.R
  if(
    !is.numeric(max.lookback) || length(max.lookback) != 1L ||
    is.na(max.lookback) || max.lookback < 0L
  )
    stop("`max.lookback` must be integer(1L), not NA, and positive")

  max.lookback <- as.integer(max.lookback)
  tmp <- tempfile()
  savehistory(tmp)
  on.exit(unlink(tmp))
  hist.rev <- rev(readLines(tmp))

  # Read from end until we reach end of file or `max.lookback` or something
  # that successfully parses to two expressions

  warn <- TRUE
  stop.line <- 0L
  parse.res.success <- NULL
  for(i in head(seq_along(hist.rev), max.lookback)) {
    lines <- rev(head(hist.rev, i))
    parse.res <- try(parse(text=lines), silent=TRUE)
    if(inherits(parse.res, "try-error")) next
    parse.res.success <- parse.res
    stop.line <- i
    if(length(parse.res) > 2L) {
      warn <- FALSE
      break
    }
  }

  if(length(parse.res.success) < 2L)
    stop(
      "Unable to retrieve final expression from last ", max.lookback,
      " history lines.  If you're sure the last command did not result ",
      "in a parse error, try increasing `max.lookback`."
    )
  if(warn) warning("We cannot guarantee that the last command was fully parsed")
  real_lines <- lines[-c(1, length(lines))]
  styler::style_text(real_lines)
}
