ui_heatmap <- function() {
  ns <- NS("heatmap")
  tabPanel("Heatmap",
    value = "heatmap",
    verticalLayout(
      plotOutput(
        ns("heatmap"),
        width = "25cm",
        height = "25cm"
      )
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

      top_matrix <- t(scaled_data())
      # Get the data matching the numeric variables that were not selected
      # If there are no columns it must be set to NULL
      if (!is.null(scaled_unselected_data())) {
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
        distance_method()
      )
    }, height = 800)
  })
}
