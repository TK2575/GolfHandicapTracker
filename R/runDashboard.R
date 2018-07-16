#' @export
runDashboard <- function() {
  appDir <- system.file("inst","shiny", package = "GolfHandicapTracker")
  if (appDir == "") {
    stop("Could not find shiny directory")
  }

  shiny::runApp(appDir, display.mode = "normal")
}
