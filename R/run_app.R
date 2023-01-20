#' return the app
#' @export
#' @importFrom shiny runApp
run_app <- function() {
  runApp(system.file("figquest", package = "figquest"))
}
