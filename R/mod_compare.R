# Cluster results comparison module

ui_compare <- function() {
  ns <- NS("compare")
  tabPanel(
    "Clusters comparison",
    value = "comparison",
    bs_accordion("compare") %>%
      bs_append(
        "Manage clusters",
        verticalLayout(
          fluidRow(
            column(
              7,
              span("Name for current results:"),
              textInput(ns("labels_name"), NULL)
            ),
            column(
              5,
              style = "margin-top: 25px",
              actionButton(ns("labels_save"), "Save results"),
              actionButton(ns("labels_clear"), "Clear results")
            )
          ),
          DT::DTOutput(ns("labels_table"), width = "20cm"),
          downloadButton(ns("labels_download"), "Download clusters")
        )
      ) %>%
      bs_append(
        "View clusters",
        verticalLayout(
          selectizeInput(ns("labels_rank"), NULL, choices = NULL),
          plotOutput(ns("labels_sankey"))
        )
      ) %>%
      shinyhelper::helper(type = "markdown", content = "compare_help")
  )
}
server_compare <- function(id, all_data, selected_data, cluster_labels) {
  moduleServer(id, function(input, output, session) {

    saved_labels <- reactiveVal(list())


    observeEvent(selected_data(), {
      updateSelectInput(session,
                        "labels_rank",
                        choices = colnames(selected_data()))
    })

    # Table of saved labels
    output$labels_table <- DT::renderDT({
      validate(need(length(saved_labels()) > 0, "No labels saved"))
      saved_labels_df <- do.call(rbind, saved_labels())
      colnames(saved_labels_df) <- c("Name", "Labels")
      saved_labels_df
    },
    rownames = TRUE,
    selection = "single",
    caption = "Click on row to select results to download",
    width = "20cm",
    options = list(scrollX = TRUE, scrollCollapse = TRUE, ordering = FALSE)
    )

    # Combine subject IDs with current cluster labels
    items_clusters_df <- reactive({
      all_df  <- all_data()
      cbind(all_df, cluster_labels())
    })

    # Save current labels
    observeEvent(input$labels_save, {
      idx <- length(saved_labels()) + 1
      current_labels <- list(name <- input$labels_name,
                             labels <- cluster_labels())
      previous_results <- saved_labels()
      previous_results[[idx]] <- current_labels
      saved_labels(previous_results)
    })

    # Clear saved labels
    observeEvent(input$labels_clear, {
      saved_labels(list())
    })

    # Download list of ids and cluster labels
    output$labels_download <- downloadHandler(
      filename = function() {
        paste0(input$labels_name, "_clusters.csv")
      },
      content = function(file) {
        utils::write.csv(items_clusters_df(), file, row.names = FALSE)
      }
    )

    output$labels_sankey <- renderPlot({
      validate(need(length(saved_labels()) > 1, "Save more results to compare"))
      df <- selected_data()

      # Reorder cluster labels based on a selected variable for comparison
      relabels <- relabel_clusters(saved_labels(), df, input$labels_rank)

      compare_df <- as.data.frame(do.call(cbind, relabels))
      names(compare_df) <- lapply(saved_labels(), function(x) x[[1]])
      compare_df$ID <- rownames(compare_df)
      compare_df <- tidyr::pivot_longer(compare_df,
                            -.data$ID,
                            names_to="Config",
                            values_to="Cluster")
      compare_df$Cluster = as.factor(compare_df$Cluster)

      plot_compare(compare_df)

    })

  })
}
