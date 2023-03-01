#' Cluster persist app
#'
#' @param seconds seconds to persist
#'
#' @export
dbutils.cluster.persist_app <- function(seconds = 3600) {
  ui <- shiny::fluidPage(
    shiny::hr(),
    shiny::actionButton("start", "Start"),
    shiny::actionButton("stop", "Stop"),
    shiny::actionButton("reset", "Reset"),
    shiny::numericInput("seconds",
      "Seconds:",
      value = seconds,
      min = 0,
      max = 99999,
      step = 1
    ),
    shiny::textOutput("timeleft")
  )

  server <- function(input, output, session) {
    timer <- shiny::reactiveVal(seconds)
    active <- shiny::reactiveVal(TRUE)

    output$timeleft <- shiny::renderText({
      paste("Time left: ", lubridate::seconds_to_period(timer()))
    })

    shiny::observe({
      shiny::invalidateLater(1000, session)
      shiny::isolate({
        if (active()) {
          timer(timer() - 1)
          if (timer() < 1) {
            shiny::stopApp()
          }
        }
      })
    })

    shiny::observeEvent(input$start, {
      active(TRUE)
    })
    shiny::observeEvent(input$stop, {
      active(FALSE)
    })
    shiny::observeEvent(input$reset, {
      timer(input$seconds)
    })
  }

  shiny::shinyApp(ui, server)
}
