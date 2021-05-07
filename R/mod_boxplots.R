ui_boxplots <- function() {
  ns <- NS("boxplots")
  tabPanel(
    "Boxplots",
    value = "boxplots",
    h5("Use the checkbox to hide clusters in the plots.
       For datasets witih more than 500 rows,
       a violin plot will be rendered instead of a boxplot"),
    verticalLayout(
      checkboxGroupInput(
        ns("boxplots_selection"),
        NULL,
        inline = TRUE
      ),
      plotOutput(
        ns("boxplots"),
        width = "100%",
        height = "auto"
      ) %>% withSpinner()
    ) %>% shinyhelper::helper(type = "markdown", content = "boxplots_help")
  )
}

server_boxplots <- function(id, selected_data, cluster_labels, cluster_colors) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    # Check if the clusters change to keep the boxplot UI updated
    observeEvent(cluster_labels(), {
      clusters <- as.list(table(cluster_labels()))
      validate(need(length(clusters) > 0, "Not ready"))
      updateCheckboxGroupInput(
        session,
        "boxplots_selection",
        choiceNames = paste(names(clusters), clusters, sep = ": "),
        choiceValues = names(clusters),
        selected = names(clusters)
      )
    })

    # Calculate boxplots for each cluster, faceting by all_data variables
    output$boxplots <- renderPlot({
      validate(need(
        length(input$boxplots_selection) > 0,
        "No clusters were selected."
      ))
      df_long <- annotate_clusters(selected_data(),
                                   cluster_labels(),
                                   TRUE,
                                   input$boxplots_selection)
      shape <- if (nrow(df_long) < selected_data()) "boxplot" else "violin"
      facet_boxplot(
        df_long, "Cluster", "Value", "Measurement", cluster_colors, shape
      )
    }, height = function() {
      ncol(selected_data()) %/% 4 * 225
    })

  })
}
