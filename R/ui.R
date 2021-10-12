#' UI
#'
#' @param request internal
#'
#' @noRd
app_ui <- function(request) {
  fluidPage(theme = "bootstrap.min.css",
  tags$head(tags$style(
    HTML(
      ".shiny-input-container-inline .shiny-options-group div { display: contents; }
         pre { white-space: break-spaces; }
        .shiny-notification-error { position: fixed; width: 250px; top: 50% ;left: 50%; }
        div.shinyhelper-container { right: 25px; }
        .shiny-output-error-validation { min-height: 50px }
        .table { white-space: nowrap; }
        tbody td:first-child { left:0; z-index: 1  }
        .align-right { margin-right: 5; float: right }
        .align-right img { vertical-align: middle;}
        .header-panel { margin-top: 5px;  margin-bottom: 5px }
      "
    )
  ),
  # Application title
  tags$title(paste(utils::packageName(), ": visual exploration of hierarchical clustering"))),
  tags$div(
    span(paste(utils::packageName(), ": visual exploration of hierarchical clustering"), class = "h2"),
    span(a(img(src = "assets/github.png", height = "20px", width = "20px"), href = "http://github.com/rhenkin/visxhclust"),
         class = "align-right"),
    class = "header-panel"
  ),
  # Sidebar -----------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      width = 3,
      ui_sidebar()
    ),
    # Main outputs ------------------------------------------------------------
    mainPanel(
      tabsetPanel(
        id = "menu",
        ui_overview(),
        ui_distancematrix(),
        ui_pca(),
        ui_heatmap(),
        ui_boxplots(),
        ui_table(),
        ui_significance(),
        ui_evaluation(),
        ui_gapstat(),
        ui_params()
      ),
      type = "hidden"
    )
  )
)
}
