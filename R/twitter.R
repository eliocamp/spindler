#' Twitter thread
#'
#' Creates a twitter thread object that can be populated with posts, published and destroyed.
#'
#' @usage
#' thread$new(tag = NULL)
#' thread$add_post(status, media = NULL)
#' thread$clear()
#' thread$publish()
#' thread$browse(n = 1)
#' thread$get_url(n = 1)
#' thread$get_posts()
#' thread$destroy()
#' thread$add_whatermark()
#' thread$preview()
#' thread$show_media(n)
#'
#' thread$save()
#' thread$load(which = -1)
#'
#'
#' @param tag String that can be used as a chunk option inside a rmarkdown document
#' to populate the thread (see Examples.)
#' @param status Text to be tweeted.
#' @param media Path to an image, video, or a ggplot2 object that will be attached to the tweet.
#' @param n Number of post in thread.
#' @param which Numeric indicating which thread to load. Negative values mean reverse
#' counting (i.e. n = -1 loads the latests thread, n = -2,the one before and so on.)
#'
#' @details
#' The basic workflow is to create a new thread object with `thread$new()` and
#' then populate it with `thread$add_post()`. `status` can be a character vector to
#' specify two posts that have different text but the same media. To add multiple pictures,
#' pass a character vector or list to `media` (the former case when combining paths and ggplot
#' objects). Keep in mind the limitations set by twitter. Each status update can have up to 4
#' static images, or 1 gif or 1 video. `spindler` will check if there's more than 4 items, but
#' not if it's an animated gif!
#'
#' You can preview it with `thread$preview()` (opens a shiny interface) or just printing it on the console.
#' Once you are happy with it, publish it to Twiter with `thread$publish()`.
#' If you want to "unpublish" it, use `thread$destroy()`. This will delete each post
#' on Twitter, but they will still be saved on your thread object.
#' Use `thread$clear()` to delete them.
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
#' You can save your threads for later use with `thread$save()`. It will write the
#' thread object to disk so you can retrieve it later. Saved posts can be retrieved
#' with the `thread$load()` function. A list of all saved threads can be retrieved
#' with the [saved_threads()] function.
#'
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
#'   publish()
#'
#' # Look at the finished product
#' birds$browse()
#'
#' # You can use the tag to populate a thread automatically from a chunk.
#' # The first figure produced by the chunk will be attached as media.
#' ```{r, setup}
#' this_thread <- spindler::thread$new(tag = "tw_status")
#' ```
#'
#' ```{r, tw_status = "The relationship between pressure and temperature is cool!"}
#' plot(pressure)
#' ```
#'
#' ```{r}
#' this_thread$add_post("This post is a free agent, not tied to any chunk.")
#' ```
#'
#' # Publish the thread rom inside the rmarkdown document.
#' # It's recomended to also save it so you can easily destroy it later
#' # in case something went wrong.
#' ```{r}
#' this_thread$publish()$save()$browse()
#' ```
#'
#' }
#'
#' @aliases new add_post browse get_url thread save load show_media preview $
#' @name twitter
NULL


#' @export
twitter <- R6::R6Class("thread", inherit = thread,
  public = list(
    publish = function() {
      if (length(self$posts) == 0) {
        message("Nothing to publish.")
        return(invisible(self))
      }

      for (p in seq_len(length(self$posts))) {
        if (p == 1) {
          prev_status <- NULL
        } else {
          prev_status <- self$posts[[p]]$id
        }
        media <- unlist(self$posts[[p]]$media)
        if (is.na(media)[1]) media <- NULL

        if (!is.null(media) & is.null(self$posts[[p]]$alt_text)) {
          self$posts[[p]]$alt_text <- rep("", length(media))
        }

        posted <- rtweet::post_tweet(status = self$posts[[p]]$status,
                                     media = media,
                                     media_alt_text = self$posts[[p]]$alt_text,
                                     in_reply_to_status_id = prev_status,
                                     auto_populate_reply_metadata = TRUE)
        self$posts[[p]]$id <- httr::content(posted)$id_str
      }

      invisible(self)
    },

    get_url = function(n = 1) {
      id <- self$posts[[n]]$id
      if (!is.na(id)) {
        paste0("https://twitter.com/", rtweet:::home_user(), "/status/", id)
      } else {
        NA
      }
    },

    destroy = function() {
      for (p in seq_len(length(self$posts))) {
        status_id <- self$posts[[p]]$id
        if (!is.na(status_id)) {
          rtweet::post_tweet(destroy_id = status_id)
          self$posts$id[p] <- NA
        }
      }
      invisible(self)
    }),
  private = list(
    validate_post = function(post) {
      problems <- vector("character")

      if (length(post$media) > 4) {
        problems <- append(problems, "* Each tweet can have at most 4 pictures.")
      }

      if (!is_tweet_length(post$status)) {
        problems <- append(problems,
                           paste0("* Status is longer than 280 characters: \n",
                                  "      ", substr(post$status, 1, 280), "..."))
      }

      if (length(problems) > 0) {
        stop("Found problems:\n", paste0(problems, collapse = "\n"))
      }

    }
  )

)


# from rtweet.
is_tweet_length <- function(.x, n = 280) {
  .x <- gsub("https?://[[:graph:]]+\\s?", "", .x)
  while (grepl("^@\\S+\\s+", .x)) {
    .x <- sub("^@\\S+\\s+", "", .x)
  }
  !(nchar(.x) <= n)   # with fix
}




#' Load all saved threads.
#'
#' Loads all the thread objects in a directory.
#'
#' @return
#' A list with [thread] objects.
#'
#'
#' @examples
#' \dontrun{
#' # Create a thread, publish it and save it.
#' thread$new()$
#'    add_post("This is a test thread!")$
#'    publish()$
#'    save()
#'
#' # List all saved threads
#' threads <- saved_threads()
#'
#' # Destroy the last thread.
#' threasd[[length(threads)]]$destroy()
#' }
#'
#' @export
saved_threads <- function() {
  dir <- rappdirs::user_data_dir("R-spindler")
  files <- list.files(dir, full.names = TRUE)
  # files <- files[seq(length(files), 1)]

  thread_list <- lapply(seq_along(files), function(f) {

    thread$new()$load(f, dir = dir)
  })
  thread_list
}

# new_thread <- function(tag = NULL) {
#   thread$new(tag = tag)
# }


