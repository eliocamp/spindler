#' Setup a knitr document to compose a thread
#'
#' Sets up hooks in a knitr document so that it will produce a twitter thread.
#'
#' @param thread thread object to populate.
#' @param publish Whether the resulting thread should be published.
#'
#' @details
#' This function should be called in the first(ish) chunks of a knitr document.
#' To add a chunk to a thread, add the option `tw_status = "Status of the tweet"`
#' to it. The first figure returned by the chunk (if any) will be attached as media.
#'

#' @examples
#' \dontrun{
#'
#' ```{r, setup}
#' this_thread <- spindler::thread$new()
#' spindler::knitr_thread(this_thread, TRUE)
#' ```
#'
#' ```{r, tw_status = "The relationship between pressure and temperature is cool!"}
#' plot(pressure)
#' ```
#' }
#'
# #' @export
knitr_thread <- function(thread, publish = FALSE) {
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("Need to install knitr.")
  }

  knitr::knit_hooks$set(tw_status = function(before, options, envir) {
    if (isFALSE(before)) {
      figure <- paste(options$fig.path, options$label, "-1.png", sep = '')
      if (!file.exists(figure)) {
        figure <- NULL
      }

      thread$add_post(status = options$tw_status,
                           media = figure)
    }
  })

  if (isTRUE(publish)) {
    knitr::knit_hooks$set(document = function(x) {
      thread$publish()$show()
      x
    })
  }
  invisible(thread)
}
