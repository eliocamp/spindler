#' Twitter thread
#'
#' Creates a twitter thread object that can be populated with posts, published and destroyed.
#'
#' @section Creating:
#' You can create a new thread with `thread$new()`. No parameters necessary.
#'
#' @section Populating:
#' Each post is added in order with `thread$add_post(status, media)`. Where `status`
#' is a length one character vector and media is a length one character vector with
#' the path to the media that will be uploaded.
#'
#' @section Cleaning up:
#' If you made a mistake or want to start over for some reason, use `thread$clear()`.
#'
#' @section Publishing:
#' When you are happy with how your thread has turned up, publish it to Twitter
#' with `thread$publish()`. The url of each tweet can be retrieved with `thread$get_url()`.
#'
#' To view the result, use `thread$show()`.
#'
#' @section Deleting:
#' To delete the thread from your timeline, use `thread$destroy()`. This will
#' delete each post on Twitter, but it will still be saved on your thread object.
#'
#'
#' @examples
#' \dontrun{
#' birds <- thread$new()
#' birds$add_post("Hey, people, I want to tell you how awesome birds are!")$
#'   add_post("They have feathers, and (most of them) can fly!")$
#'   add_post("And look how cute they ares", media = "pictures/birds/penguin1.png")
#'
#' birds$publish()
#'
#' # Oh, no I made a typo. Quick, delete the whole thing!
#' birds$destroy()
#'
#' # Let's start over
#' birds$clear()$
#'   add_post("Nooo! I had an awesome thread about birds, but I messed up.")$
#'   add_post("So here's the jist of it: birds rock and they are better than monkeys!")$
#'   publish
#'
#' # Look at the finished product
#' birds$show()
#' }
#'
#' @usage NULL
#' @format NULL
#'
#'
#' @export
thread <- R6::R6Class("tweet_thread", list(
  add_post = function(status, media, order) {
    last <- length(self$post_list)
    if (missing(status)) status <- NULL
    if (missing(media)) media <- NULL

    if (!rtweet:::is_tweet_length(status)) {
      stop("Status longer than 280 characters:\n  * ", substr(status, 1, 140), "...")
    }

    if (!all(is.null(media), is.null(status))){
      self$post_list[[last + 1]] <- list(status = status,
                                         media = media,
                                         id = NA)
    }
    invisible(self)
  },

  publish = function() {
    if (length(self$post_list) == 0) {
      message("Nothing to publish.")
      return(invisible(self))
    }

    for (p in seq_along(self$post_list)) {
      if (p == 1) {
        prev_status <- NULL
      } else {
        prev_status <- self$post_list$id[p - 1]
      }

      rtweet::post_tweet(status = self$post_list[[p]]$status,
                         media = self$post_list[[p]]$media,
                         in_reply_to_status_id = prev_status,
                         auto_populate_reply_metadata = TRUE)
      my_timeline <- rtweet::get_timeline(rtweet:::home_user())
      self$post_list$id[p] <-  my_timeline$status_id[1]
    }

    invisible(self)
  },

  get_url = function(n = 1) {
    id <- self$post_list$id[n]
    if (!is.na(id)) {
      paste0("https://twitter.com/", rtweet:::home_user(), "/status/", id)
    } else {
      NA
    }
  },

  show = function(n = 1) {
    url <- self$get_url(n)
    if (is.na(url)) {
      stop("Tweet not published yer.")
    }
    browseURL(url)
    invisible(self)
  },

  destroy = function() {
    for (p in seq_along(self$post_list)) {
      status_id <- self$post_list$id[p]
      if (!is.na(status_id)) {
        rtweet::post_tweet(destroy_id = status_id)
        self$post_list$id[p] <- NA
      }
    }
  },

  clear = function() {
    self$post_list <- NULL
  },

  post_list = NULL
))



