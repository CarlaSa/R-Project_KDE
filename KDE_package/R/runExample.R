#' Run the shiny app.
#'
#' @export
runExample <- function() {
  appDir <- system.file("shiny-examples", "KDE_app", package = "KDE")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `KDE`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
