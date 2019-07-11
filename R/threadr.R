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
#' thread$add_whatermark(lang = "en")
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
#' @param lang Language of the watermark. Currently "en" (English) or "es" (Spanish).
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
#' @aliases new add_post browse get_url thread save load show_media preview $ add_whatermark
#' @name thread
NULL


#' @export
thread <- R6::R6Class("tweeter_thread", list(
  initialize = function(tag = NULL) {

    if (!is.null(tag)) {
      if (!requireNamespace("knitr", quietly = TRUE)) {
        stop("Need to install knitr.")
      }

      knitr_status <- function(before, options, envir) {
        if (isTRUE(before)) {
          options$dev <- c(options$dev, "png")
        }

        if (isFALSE(before)) {
          media <- paste(options$fig.path, options$label, "-1.png", sep = '')
          if (!file.exists(media)) {
            media <- NULL
          }

          self$add_post(status = options[[tag]],
                        media = figure)
        }
      }

      do.call(knitr::knit_hooks$set, setNames(list(knitr_status), tag))

      # if (isTRUE(publish)) {
      #   knitr::knit_hooks$set(document = function(x) {
      #     self$publish()$browse()
      #     self$save()
      #     x
      #   })
      # }
    }
    invisible(self)
  },


  add_post = function(status, media = NULL) {

    # render all media
    if (is.null(media)) {
      media <- NA  # NAs work better for storage
    } else {
      if (inherits(media, c("ggplot", "gganim"))) {
        media <- list(media)
      }
      media <- vapply(media, function(m) {
        # Render plot if needed
        #
        if (inherits(m, "gganim"))  {
          if (!requireNamespace("gganimate", quietly = TRUE)) {
            stop('gganimate package required to render ggplot 2 animations. Install it with `install.packages("gganimate")`')
          }
          filename <- tempfile(pattern = "twitter_plot_", fileext = ".gif")
          dpi <- 150
          gganimate::anim_save(filename = filename, animation = m,
                               width = 800,
                               height = 400)
          m <- filename
        }
        if (inherits(m, "ggplot")) {
          if (!requireNamespace("ggplot2", quietly = TRUE)) {
            stop('ggplot2 package required to render ggplot2 objects. Install it with `install.packages("ggplot2")`')
          }
          filename <- tempfile(pattern = "twitter_plot_", fileext = ".png")
          dpi <- 150
          ggplot2::ggsave(plot = m, filename = filename,
                          width = 1024/dpi,
                          height = 512/dpi, dpi = dpi)
          m <- filename
        } else {
          m <- normalizePath(m)
        }
        if (!file.exists(m)) {
          stop("No media in ", m)
        }
        m
      }, "char")

      if (length(media) > 4) {
        stop("Each tweet can have at most 4 pictures.")
      }
    }

    # Each status is one post and contains all media
    for (s in seq_along(status)) {

      if (!is_tweet_length(status[s])) {
        stop("Status longer than 280 characters:\n  * ", substr(status, 1, 140), "...")
      }


      new_post <- data.frame(id = NA,
                             status = status[s],
                             stringsAsFactors = FALSE)
      new_post[["media"]] <- list(media)

      self$posts <-  rbind(self$posts, new_post)

    }

    invisible(self)
  },

  add_watermark = function(lang = "en") {
    self$add_post(status = self$watermark[[lang]])
  },

  publish = function() {
    if (nrow(self$posts) == 0) {
      message("Nothing to publish.")
      return(invisible(self))
    }

    for (p in seq_len(nrow(self$posts))) {
      # browser()
      if (p == 1) {
        prev_status <- NULL
      } else {
        prev_status <- self$posts$id[p - 1]
      }
      media <- unlist(self$posts$media[p])
      if (is.na(media)[1]) media <- NULL

      rtweet::post_tweet(status = self$posts$status[p],
                         media = media,
                         in_reply_to_status_id = prev_status,
                         auto_populate_reply_metadata = TRUE)
      my_timeline <- rtweet::get_timeline(rtweet:::home_user())
      self$posts$id[p] <-  my_timeline$status_id[1]
    }

    invisible(self)
  },

  get_url = function(n = 1) {
    id <- self$posts$id[n]
    if (!is.na(id)) {
      paste0("https://twitter.com/", rtweet:::home_user(), "/status/", id)
    } else {
      NA
    }
  },

  get_posts = function() {
    return(self$posts)
  },

  preview = function() {
    thread_show(self)
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
    for (p in seq_len(nrow(self$posts))) {
      status_id <- self$posts$id[p]
      if (!is.na(status_id)) {
        rtweet::post_tweet(destroy_id = status_id)
        self$posts$id[p] <- NA
      }
    }
    invisible(self)
  },

  clear = function() {
    self$posts <- data.frame(id = NULL, status = NULL, media = NULL, stringsAsFactors = FALSE)
    invisible(self)
  },

  print = function() {
    if (nrow(self$posts) == 0) {
      message("Empty thread.")
      return(invisible(self))
    }

    posts <- self$get_posts()
    n <- nrow(posts)

    max_width <- nchar(n)
    margin <- strrep(" ", max_width + 2)

    for (p in seq_len(nrow(posts))) {
      cat(formatC(p, width = max_width), ": ", sep = "")
      post <- strsplit(posts$status[p], "\n")[[1]]
      post <- strwrap(post, width = 60)

      cat(paste0(post, collapse = paste0("\n", margin, collapse = ""), sep = ""))

      if (!is.na(posts$media[p])) {
        for (m in unlist(posts$media[p])) {
          cat("\n")
          cat(margin, m, sep = "")
        }


      }
      cat("\n")
      cat(margin, "| \n", sep = "")
    }
  },

  show_media = function(n) {
    media <- unlist(self$get_posts()$media[n])

    if (is.na(media[1])) {
      return(NA)
    }

    file.show(media)

    invisible(media)
  },

  save = function(force = FALSE) {
    dir <- rappdirs::user_data_dir("R-spindler")

    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

    file_name <- file.path(dir, paste0(as.character(Sys.time()), ".Rds"))

    saveRDS(self, file_name)

    invisible(self)
  },

  load = function(which = -1) {
    dir <- rappdirs::user_data_dir("R-spindler")
    files <- list.files(dir, full.names = TRUE)

    if (sign(which) == -1) {
      which <- length(files) + which + 1
    }

    readRDS(files[which])
  },

  watermark = list(
    en = "This thread comes to you courtesy of the spindler \U1F4E6. \nReproducible tweets with R and rmarkdown. \n#rstats \nhttps://git.io/fjzxN",
    es = "Este hilo es cortes\u00eda del \U1F4E6 spindler.\n Twits reproducibles con R y rmarkdown. \n#rstats #rstatsES \nhttps://git.io/fjzxN"),

  posts = data.frame(id = NULL, status = NULL, media = NULL, stringsAsFactors = FALSE)
))


# from rtweet.
is_tweet_length <- function(.x, n = 280) {
  .x <- gsub("https?://[[:graph:]]+\\s?", "", .x)
  while (grepl("^@\\S+\\s+", .x)) {
    .x <- sub("^@\\S+\\s+", "", .x)
  }
  nchar(.x) <= n
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

