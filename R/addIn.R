
png_from_clipboard <- function() {
  sys <- Sys.info()["sysname"]
  img <- tempfile(pattern = "spindler_clip_", fileext = ".png")

  if (sys == "Windows") {
    #Windows

  } else if (sys == "Darwin") {
    # OSX
  } else {
    # Linux
    targets <- try(system("xclip -selection clipboard -t TARGETS -o", intern = TRUE), silent = TRUE)


    if (inherits(target, "try-error")) {
      warning("Clipboard on X11 requires 'xclip'.")
    }

    if ("image/png" %in% targets) {
      system(paste0("xclip -selection clipboard -t image/png -o > ", img))
    }
    else {
       warning("No image in clipboard")
      img <- ""
     }
  }
  return(img)
}


imgFromClipboard <- function() {
  img <- png_from_clipboard()
  if (img != "") rstudioapi::insertText(paste0('"', img, '"'))
}

