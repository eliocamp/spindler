#' Render media
#'
#' Methods for rendering custom content.
#'
#' @param object R object to render.
#' @param ... Other arguments passed to the renderer function.
#'
#' @return
#' The path to the rendered media.
#'
#'
#' @export
spindler_render <- function(object, ...) {
  UseMethod("spindler_render")
}

#' @export
spindler_render.character <- function(object, ...) {
  object
}


#' @export
spindler_render.gganim <- function(object, width= 800, height = 400, ...) {
  if (!requireNamespace("gganimate", quietly = TRUE)) {
    stop('gganimate package required to render ggplot 2 animations. Install it with `install.packages("gganimate")`')
  }
  filename <- tempfile(pattern = "spindler_plot_", fileext = ".gif")
  dpi <- 72
  gganimate::anim_save(animation = object,
                       filename = filename,
                       width = width,
                       height = height,
                       ...)
  filename
}

#' @export
spindler_render.ggplot <- function(object, width = 1024/dpi, height = 512/dpi, dpi = 72, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop('ggplot2 package required to render ggplot2 objects. Install it with `install.packages("ggplot2")`')
  }
  filename <- tempfile(pattern = "spindler_plot_", fileext = ".png")
  # dpi <- 72
  ggplot2::ggsave(plot = object,
                  filename = filename,
                  width = width,
                  height = height,
                  dpi = dpi,
                  ...)
  filename
}

#' @export
spindler_render.gtable <- function(object, ...) {
  spindler_render.ggplot(object, ...)
}


#' @export
spindler_render.Carbon <- function(object, ...) {
  filename <- tempfile(pattern = "spindler_code_",
                       fileext = ".png")
  object$carbonate(file = basename(filename))
  object$stop()
  filename
}

#' @export
spindler_render.default <- function(object, ...) {
  stop("No spindler_renderer method for object of class ", paste0(class(object), collapse = " "))
}
