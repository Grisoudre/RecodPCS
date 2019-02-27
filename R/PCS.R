#' @export
PCS <- function() {
  appDir <- system.file("AppPCS", "myapp", package = "RecodPCS")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `RecodPCS`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
