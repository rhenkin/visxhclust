ui_pca <- function() {
  ns <- NS("pca")
  tabPanel("PCA",
    value = "pca",
    splitLayout(
      plotOutput(ns("pca_plot"), width = "100%", height = "400px"),
      plotOutput(ns("drivers_plot"), width = "100%", height = "auto"),
      cellWidths = c("40%", "60%")
    ) %>% shinyhelper::helper(type = "markdown", content = "pca_help")
  )
}

server_pca <- function(id, selected_data, cluster_labels, cluster_colors) {
  moduleServer(id, function(input, output, session) {

    pca_data <- reactive({
      df <- selected_data()
      stats::prcomp(df)
    })

    output$pca_plot <- renderPlot({
      pca_scatterplot(pca_data(), cluster_labels(), cluster_colors)
    })

    output$drivers_plot <- renderPlot({
      drivers_df <- pca_drivers_df(selected_data(), pca_data())
      pca_driversplot(drivers_df)
    }, height = function() ncol(selected_data()) * 20)

  })
}
