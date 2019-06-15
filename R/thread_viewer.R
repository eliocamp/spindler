library(shiny)
library(miniUI)

myGadgetFunc <- function(inputValue1, inputValue2) {

  text1 <- "logo"
  text2 <- "logo2"
  image1 <- "logo/logo.png"

  style_card <- "border:1px solid gray;box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);border-radius: 3px;color:#d4d4d4"
  ui <- miniPage(
    gadgetTitleBar("Thread"),
    miniContentPanel(

      div(class = "tweet", style = style_card,
          p(class = "status", text1),
          img(class = "image", src = paste0(getwd(), "logo/logo.png"))
      ),
      div(class = "tweet", style = style_card,
          p(class = "status", text2))


      # Define layout, inputs, outputs
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
