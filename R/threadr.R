#' Twitter thread
#'
#' Creates a twitter thread object that can be populated with posts, published and destroyed.
#'
#' @usage
#' thread$new(tag = NULL, publish = FALSE)
#' thread$add_post(status, media = NULL)
#' thread$clear()
#' thread$publish()
#' thread$browse(n = 1)
#' thread$get_url(n = 1)
#' thread$get_posts()
#' thread$destroy()
#' thread$add_whatermark()
#'
#' @param tag String that can be used as a chunk option inside a rmarkdown document
#' to populate the thread (see Examples.)
#' @param publish Wheter to publish the thread when finishing rendering. Only used
#' when rendering a rmarkdown document.
#' @param status Text to be tweeted.
#' @param media Path to an image or video that will be attached to the tweet.
#' @param n Number of post in thread to show in browser or get url from.
#'
#' @details
#' The basic workflow is to create a new thread object with `thread$new()` and
#' then populate it with `thread$add_post()`. Once you are happy with it, publish
#' it to Tweeter with `thread$publish()`. If you want to "unpublish" it, use
#' `thread$destroy()`. This will delete each post on Twitter, but they will
#' still be saved on your thread object. Use `thread$clear()` to delete them.
#'
#' To view the list of posts currently on your thread, use `thread$get_posts()`.
#' Once published, you can get the url of each post with `thread$get_url()` or
#' open it up in a browser session with `thread$browse()`.
#'
#' The `thread$add_watermark()` method adds this post:
#'
#' This thread comes to you courtesy of the spindler {package emoji} \cr
#' Reproducible tweets with R and rmarkdown. \cr
#' \#rstats \cr
#' https://git.io/fjzxN
#'
#' If you like the package, consider adding it so more people can enjoy it :\).
#'
#' @examples
#' \dontrun{
#' birds <- thread$new()
#' birds$add_post("Hey, people, I want to tell you how awesome birds are!")$
#'   add_post("They have feathers, and (most of them) can fly!")$
#'   add_post("And look how cute they ares", media = "~/Pictures/penguin1.png")
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
#' birds$browse()
#'
#' # You can use the tag to populate a thread automatically from a chunk.
#' # The first figure produced by the chunk will be attached as media.
#' ```{r, setup}
#' this_thread <- spindler::thread$new(tag = "tw_status", publish = TRUE)
#' ```
#'
#' ```{r, tw_status = "The relationship between pressure and temperature is cool!"}
#' plot(pressure)
#' ```
#'
#' ```{r}
#' this_thread$add_post("This post is a free agent, not tied to any chunk.")
#' ```
#' }
#'
#' @aliases new add_post browse get_url thread $
#' @name thread
NULL


#' @export
thread <- R6::R6Class("tweeter_thread", list(
  initialize = function(tag = NULL, publish = FALSE) {

    if (!is.null(tag)) {
      if (!requireNamespace("knitr", quietly = TRUE)) {
        stop("Need to install knitr.")
      }

      knitr::knit_hooks$set(tw_status = function(before, options, envir) {
        if (isTRUE(before)) {
          options$dev <- c(options$dev, "png")
        }

        if (isFALSE(before)) {
          figure <- paste(options$fig.path, options$label, "-1.png", sep = '')
          if (!file.exists(figure)) {
            figure <- NULL
          }

          self$add_post(status = options$tw_status,
                        media = figure)
        }
      })

      if (isTRUE(publish)) {
        knitr::knit_hooks$set(document = function(x) {
          self$publish()$browse()
          x
        })
      }
    }
    invisible(self)
  },

  add_post = function(status, media = NULL) {
    last <- length(self$post_list)
    # if (missing(status)) status <- NULL

    if (!is_tweet_length(status)) {
      stop("Status longer than 280 characters:\n  * ", substr(status, 1, 140), "...")
    }

    if (!is.null(media) && !file.exists(media)) {
      stop("No media in ", media)
    }

    if (!all(is.null(media), is.null(status))){
      self$post_list[[last + 1]] <- list(status = status,
                                         media = media,
                                         id = NA)
    }
    invisible(self)
  },

  add_watermark = function() {
    self$add_post(status = self$watermark)
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
    id <- self$post_list[[n]]$id
    if (!is.na(id)) {
      paste0("https://twitter.com/", rtweet:::home_user(), "/status/", id)
    } else {
      NA
    }
  },

  get_posts = function() {
    self$post_list
  },

  browse = function(n = 1) {
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

  print = function() {
    posts <- self$get_posts()
    n <- length(posts)

    max_width <- nchar(n)
    margin <- strrep(" ", max_width + 2)

    for (p in seq_along(posts)) {
      cat(formatC(p, width = max_width), ": ", sep = "")

      post <- strsplit(posts[[p]]$status, "\n")[[1]]
      post <- strwrap(post, width = 60)

      cat(paste0(post, collapse = paste0("\n", margin, collapse = ""), sep = ""))

      if (!is.null(posts[[p]]$media)) {
        cat("\n")
        cat(margin, normalizePath(posts[[p]]$media), sep = "")
      }
      cat("\n")
      cat(margin, "| \n", sep = "")
    }
  },

  watermark = "This thread comes to you courtesy of the spindler \U1F4E6. \nReproducible tweets with R and rmarkdown. \n#rstats \nhttps://git.io/fjzxN",

  post_list = NULL
))


# from rtweet.
is_tweet_length <- function(.x, n = 280) {
  .x <- gsub("https?://[[:graph:]]+\\s?", "", .x)
  while (grepl("^@\\S+\\s+", .x)) {
    .x <- sub("^@\\S+\\s+", "", .x)
  }
  nchar(.x) <= n
}

