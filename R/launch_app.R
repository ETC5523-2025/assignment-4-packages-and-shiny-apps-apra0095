#' Launch the embedded Shiny app
#' @export
launch_app <- function() {
  app_dir <- system.file("app", package = utils::packageName())
  if (identical(app_dir, "") || !dir.exists(app_dir))
    stop("App directory not found: create inst/app/app.R and reinstall.", call. = FALSE)
  shiny::runApp(app_dir, display.mode = "normal")
}
