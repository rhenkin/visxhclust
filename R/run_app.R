#' @importFrom magrittr %>%
#' @import shiny
#' @import ggplot2
#' @import ggalluvial
#' @importFrom shinycssloaders withSpinner
#' @importFrom bsplus bs_append bs_set_opts bs_accordion
#' @importFrom shinyhelper helper observe_helpers
NULL

#' Runs the Shiny app
#'
#' @export
#' @examples
#' \dontrun{
#' library(visxhclust)
#' run_app()
#' }
run_app <- function() {
  shiny::shinyApp(app_ui, app_server,options = list(launch.browser = TRUE))
}
