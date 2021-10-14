ui_heatmap <- function() {
  ns <- NS("heatmap")
  tabPanel("Heatmap",
    value = "heatmap",
    verticalLayout(
      plotOutput(
        ns("heatmap"),
        width = "25cm",
        height = "auto"
      ),
      checkboxInput(ns("cluster_features"),
                    label = "Cluster features on heatmap?",
                    value = TRUE),
      checkboxInput(ns("hide_unselected"),
                    label = "Display unselected features?",
                    value = TRUE),
      checkboxInput(ns("show_col_names"),
                    label = "Display record IDs labels?",
                    value = FALSE)
    )
  )
}

server_heatmap <- function(id,
                           heatmap_annotation,
                           clusters,
                           nclusters,
                           cluster_colors,
                           scaled_data,
                           scaled_unselected_data,
                           scale_flag,
                           distance_method) {

  moduleServer(id, function(input, output, session) {
    output$heatmap <- renderPlot({
      req(scaled_data())
      top_matrix <- t(scaled_data())
      # Get the data matching the numeric variables that were not selected
      # If there are no columns it must be set to NULL
      if (!is.null(scaled_unselected_data()) & input$hide_unselected) {
        bottom_matrix <- t(scaled_unselected_data())
      } else {
        bottom_matrix <- NULL
      }

      heatmap_clusters <- reorder_dendrograms(clusters(),
                                              nclusters(), cluster_colors)
      plot_cluster_heatmaps(
        top_matrix,
        bottom_matrix,
        heatmap_clusters$dendrogram,
        heatmap_clusters$ids,
        heatmap_annotation(),
        scale_flag(),
        distance_method(),
        input$cluster_features,
        input$show_col_names
      )
    }, height = function() { if (ncol(scaled_data()) > 100) 900 else 700 })
  })
}
