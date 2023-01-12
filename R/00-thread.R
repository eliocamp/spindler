
#' @export
thread <- R6::R6Class("thread",
  public = list(
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

    add_post = function(status, media = NULL,
                        alt_text = NULL,
                        sensitive = FALSE,
                        spoiler_text = NULL,
                        visibility = "public",
                        scheduled_at = NULL,
                        language = NULL,
                        ...) {
      # render all media
      if (is.null(media)) {
        media <- NA  # NAs work better for storage
      } else {
        if (inherits(media, c("ggplot", "gganim", "R6", "gtable"))) {
          media <- list(media)
        }

        # Render items
        media <- vapply(media, function(x) spindler_render(x, ...), "char")

        # Normalize paths and check that files exist
        media <- vapply(media, function(m) {
          m <- normalizePath(m)
          if (!file.exists(m)) {
            stop("No media in ", m)
          }
          m
        }, "char")

        # move to twitter
        # if (length(media) > 4) {
        #   stop("Each tweet can have at most 4 pictures.")
        # }
      }

      # Each status is one post and contains all media
      for (s in seq_along(status)) {
        new_post <- list(id = NA,
                         status = status[s],
                         sensitive = sensitive,
                         spoiler_text = spoiler_text,
                         visibility = visibility,
                         scheduled_at = scheduled_at,
                         alt_text = alt_text,
                         language = language)
        new_post[["media"]] <- list(media)

        private$validate_post(new_post)

        self$posts[[length(self$posts)+1]] <-  new_post
      }

      invisible(self)
    },

    add_watermark = function() {
      self$add_post(status = self$watermark)
    },

    publish = function(platform = c("mastodon", "twitter")) {

      thread <- switch (platform,
        mastodon = mastodon$new(),
        twitter = twitter$new()
      )

      thread$posts <- self$posts
      thread$publish()

      return(thread)
    },

    get_url = function(n = 1) {
      stop("Not implemented")
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
        stop("Post not published yer.")
      }
      browseURL(url)
      invisible(self)
    },

    destroy = function() {
     stop("Not implemented")
    },

    clear = function() {
      self$posts <- data.frame(id = NULL, status = NULL, media = NULL, stringsAsFactors = FALSE)
      invisible(self)
    },

    print = function() {

      if (length(self$posts) == 0) {
        message("Empty thread.")
        return(invisible(self))
      }

      posts <- self$get_posts()
      n <- length(posts)

      max_width <- nchar(n)
      margin <- strrep(" ", max_width + 2)

      for (p in seq_len(length(posts))) {
        cat(formatC(p, width = max_width), ": ", sep = "")
        post <- strsplit(posts[[p]]$status, "\n")[[1]]
        post <- strwrap(post, width = 60)

        cat(paste0(post, collapse = paste0("\n", margin, collapse = ""), sep = ""))

        if (!is.na(posts[[p]]$media)) {
          for (m in unlist(posts[[p]]$media)) {
            cat("\n")
            cat(margin, m, sep = "")
          }


        }
        cat("\n")
        cat(margin, "| \n", sep = "")
      }
    },

    show_media = function(n) {
      media <- unlist(self$get_posts()[[n]]$media)

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

    watermark = "This thread comes to you courtesy of the spindler \U1F4E6. \nReproducible tweets with R and rmarkdown. \n#rstats \nhttps://git.io/fjzxN",

    posts = list()
  ),

  private = list(
    validate_post = function(post) {
      return(NULL)
    }
  ))



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
