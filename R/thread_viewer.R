
get_user_avatar <- function()  {
  user <- rtweet:::home_user()
  rtweet::lookup_users(user)$profile_image_url
}

status_card <- function(post, avatar, p) {
  style_card <- "display:flex;background-color:#fafafa;padding:5px;margin-bottom: 10px;border:1px solid gray;box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);border-radius: 3px;color:#d4d4d4"
  style_p <- "color:Black;"
  if (!is.na(post$media)) {
    media <- unlist(post$media)

    width_media <- paste0(80/length(media), "%")

    imgs <- lapply(seq_along(media), function(m) {
      resource <- paste0("images", p, m)
      shiny::addResourcePath(resource, dirname(media[m]))
      shiny::img(width = width_media, src = file.path(resource, basename(media[m])))
    })

    media_div <- shiny::div(class = "image-container",
                            imgs)
  } else {
    media_div <- NULL
  }

  avatar_div <-  shiny::div(class = "avatar-container", style = "padding:4px;flex:10%",
                            shiny::img(src = avatar, style="border-radius:50%"))

  status_div <- shiny::div(class = "status-container", style = "padding:2px;flex:90%",
                           shiny::p(class = "status", style = style_p, post$status),
                           media_div)

  shiny::div(class = "tweet", style = style_card,
             avatar_div,
             status_div)
}

thread_show <- function(thread) {
  if (!requireNamespace("shiny", quietly = TRUE) | !requireNamespace("miniUI", quietly = TRUE)) {
    stop('Thread preview needs the shiny and miniUI packages, install them with `install.packages(c("shiny", "miniUI"))` and try again')
  }

  posts <- thread$get_posts()

  avatar <- get_user_avatar()
  posts_divs <- lapply(seq_len(nrow(posts)), function(p) status_card(posts[p, ], avatar, p))
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Thread", right = miniUI::miniTitleBarButton("publish", "Publish Thread", primary = TRUE)),
    miniUI::miniContentPanel(padding = 20,
                             posts_divs
    )
  )

  server <- function(input, output, session) {
    # Define reactive expressions, outputs, etc.

    # When the Done button is clicked, return a value
    shiny::observeEvent(input$publish, {
      thread$publish()
      shiny::stopApp(invisible(thread))
    })
  }

  shiny::runGadget(ui, server, viewer = shiny::dialogViewer("Thread"))
}
