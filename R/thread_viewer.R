library(shiny)
library(miniUI)

get_user_avatar <- function()  {
  user <- rtweet:::home_user()
  rtweet::lookup_users(user)$profile_image_url
}

myGadgetFunc <- function(thread) {
  style_card <- "background-color:#fafafa;padding:5px;margin-bottom: 10px;border:1px solid gray;box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);border-radius: 3px;color:#d4d4d4"
  style_p <- "color:Black;"
  posts <- thread$get_posts()
  avatar <- get_user_avatar()
  posts_divs <- lapply(seq_len(nrow(posts)), function(p) {

    if (is.na(posts$media[p])) {
      div(class = "tweet", style = style_card,
          img(src = avatar),
          div(class = "status-container",
              p(class = "status", style = style_p, posts$status[p]))
      )

    } else {
      div(class = "tweet", style = style_card,
          img(src = avatar),
          div(class = "status-container",
              p(class = "status", style = style_p, posts$status[p])),
          div(class = "image-container",
              img(class = "image", src = posts$media[p])
          )
      )
    }
  })

  ui <- miniPage(
    gadgetTitleBar("Thread"),
    miniContentPanel( padding = 50,
                      posts_divs
    )
  )

  server <- function(input, output, session) {
    # Define reactive expressions, outputs, etc.

    # When the Done button is clicked, return a value
    observeEvent(input$done, {
      returnValue <- NULL
      stopApp(returnValue)
    })
  }

  runGadget(ui, server)
}
