ui_significance <- function() {
  ns <- NS("signif")
  tabPanel(
    "Significance testing",
    value = "signific",
    h5("Test significance for variables not used in clustering"),
    verticalLayout(
      selectizeInput(
        ns("signif_var"),
        "Select variable",
        choices = NULL,
        options = list(
          onInitialize = I('function(){this.setValue("");}')
        )
      ) %>% shinyhelper::helper(type = "markdown", content = "signif_help"),
      splitLayout(
        plotOutput(ns("signif_boxplot"), height = 700),
        htmlOutput(ns("signif_results"), container = pre),
        cellWidths = c("25%", "75%")
      )
    )
  )
}
server_significance <- function(id, all_data, cluster_labels, cluster_colors,
                             unselected_vars) {
  moduleServer(id, function(input, output, session) {

    observeEvent(unselected_vars(), {
      updateSelectizeInput(session,
                           "signif_var",
                           choices = unselected_vars())
    })

    output$signif_boxplot <- renderPlot({
      req(input$signif_var)
      isolate({ all_df <- all_data() })
      df <- all_df[, input$signif_var]
      df$Cluster <- as.factor(cluster_labels())
      facet_boxplot(df, "Cluster", input$signif_var,
                    boxplot_colors = cluster_colors, plot_points = FALSE)
    }, height = 150)


    # Compute Dunn or t-test
    # Dunn is only valid when there are more than 3 clusters
    output$signif_results <- renderPrint({
      req(input$signif_var)
      isolate({ all_df <- all_data() })
      clusters <- cluster_labels()
      num_clusters <- length(unique(clusters))

      if (num_clusters > 2) {
        dunn.test::dunn.test(
          x = dplyr::pull(all_df, input$signif_var),
          g = clusters,
          method = "bh"
        )
      }
      else {
        all_df$Cluster <- clusters
        t_test_formula <-
          stats::as.formula(paste(input$signif_var, " ~ Cluster"))
        stats::t.test(t_test_formula, data = all_df)
      }
    })
  })
}
