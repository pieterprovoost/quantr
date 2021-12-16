#' Run the eight pillars app
#'
#' @export
run_pillars <- function() {
  app_dir <- system.file("shiny", "pillars", package = "quantr")
  shiny::runApp(app_dir, display.mode = "normal")
}
