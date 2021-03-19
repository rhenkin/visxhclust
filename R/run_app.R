#' @importFrom magrittr %>%
#' @import shiny
#' @importFrom shinycssloaders withSpinner
#' @importFrom bsplus bs_append bs_set_opts bs_accordion
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
  shiny::shinyApp(app_ui, app_server)
}
