#' @importFrom magrittr %>%
#' @import shiny
#' @import ggplot2
#' @import dplyr
#' @import tidyselect
#' @importFrom shinycssloaders withSpinner
#' @importFrom bsplus bs_append bs_set_opts bs_accordion
#' @importFrom shinyhelper helper
#'
#'
NULL

#' Function to run app
#'
#' @export
run_app <- function() {
  shiny::shinyApp(app_ui, app_server)
}
