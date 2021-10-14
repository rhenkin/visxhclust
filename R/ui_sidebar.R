ui_sidebar <- function() {
  verticalLayout(
    fileInput(
      "file1",
      "Choose file:",
      multiple = FALSE,
      accept = c(".rds", ".csv", ".tsv", ".txt")
    ) %>%
      shinyhelper::helper(type = "markdown", content = "sidebar_help"),
    checkboxInput("scaling", "Scale data?", value = TRUE),
    #radioButtons(
    #  "scaling",
    #  "Scaling:",
    #  choices = c("Z-scores", "Robust", "None")
    #),
    div(
      id = "sidebar",
      selectInput(
        "distance_method",
        p("Distance method:"),
        choices = c(
          "Euclidean",
          "Manhattan",
          "Mahalanobis",
          "Cosine",
          "Maximum",
          "Canberra",
          "Minkowski",
          "Binary"
        ),
        selected = "Euclidean"
      ),
      selectInput(
        "linkage_method",
        p("Linkage method:"),
        choices = c("single", "complete", "average", "ward.D2"),
        selected = "ward.D2"
      ),
      numericInput(
        "nclusters",
        p("No. of clusters:"),
        value = 2,
        min = 1,
        step = 1
      ),
      downloadButton("download_clusters",
                   "Download data with clusters"),
      tags$br(),tags$br(),
      bs_accordion(id = "cluster_variable_selection") %>%
        bs_set_opts(panel_type = "default") %>%
        bs_append(
          title = "Numeric features",
          content = tagList(
          tags$span(
            actionButton("flip_numeric", "Flip selection"),
            actionButton("select_all", "Select all")),
            checkboxGroupInput(
              "selected_numeric",
              NULL,
              choices = NULL,
              selected = NULL
            )
          )
        ) %>%
        bs_append(
          title = "Heatmap features",
          content = checkboxGroupInput("selected_annotation",
            NULL,
            choices = NULL
          )
        ),
      numericInput(
        "corr_threshold",
        p("Correlation threshold for automatic removal:"),
        value = 0.9,
        step = 0.01
      )
    )
  )
}
