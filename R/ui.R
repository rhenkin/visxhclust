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
        .shiny-output-error-validation { min-height: 50px }"
    )
  )),
  # Application title
  titlePanel(paste(utils::packageName(), ": visual exploration of hierarchical clustering")),
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
        ui_compare(),
        ui_params()
      ),
      type = "hidden"
    )
  ),
  fluidRow(
    div(
      style = "margin: 15px",
      "Created at the",
      a(
        "Centre for Translational Bioinformatics",
        href = "https://www.qmul.ac.uk/c4tb/",
        target = "blank"
      ),
      "at Queen Mary, University of London."
    )
  )
)
}
