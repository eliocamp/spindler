#' @export
mastodon <- R6::R6Class("thread_mastodon", inherit = thread,
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
          prev_status <- self$posts[[p-1]]$id
        }
        media <- unlist(self$posts[[p]]$media)
        if (is.na(media)[1]) media <- NULL

        if (!is.null(media) & is.null(self$posts[[p]]$alt_text)) {
          self$posts[[p]]$alt_text[p] <- rep("", length(media))
        }

browser()
        posted <- rtoot::post_toot(status = self$posts[[p]]$status,
                                   media = unname(media),
                                   alt_text = self$posts[[p]]$alt_text,
                                   sensitive = self$posts[[p]]$sensitive,
                                   spoiler_text = self$posts[[p]]$spoiler_text,
                                   visibility = self$posts[[p]]$visibility,
                                   scheduled_at = self$posts[[p]]$scheduled_at,
                                   language = self$posts[[p]]$language,
                                   in_reply_to_id = prev_status
        )

        self$posts[[p]]$id <- httr::content(posted)$id
      }

      invisible(self)
    }),
  private = list(
    validate_post = function(post) {
      return(NULL)
    }
  )
)

