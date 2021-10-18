#' @importFrom dplyr %>%
#' @import shiny
#' @import ggplot2
#' @importFrom shinycssloaders withSpinner
#' @importFrom bsplus bs_append bs_set_opts bs_accordion
#' @importFrom shinyhelper helper observe_helpers
NULL

#' Runs the Shiny app
#'
#' @return No return value, runs the app by passing it to print
#' @export
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#' library(visxhclust)
#' run_app()
#' }
run_app <- function() {
  addResourcePath("assets", system.file("www", package = "visxhclust") )
  shiny::shinyApp(app_ui, app_server,options = list(launch.browser = TRUE))
}
