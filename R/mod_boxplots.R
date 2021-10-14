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
      bs_accordion(ns("cboxplots")) %>%
        bs_append("Boxplots",
                  plotOutput(
        ns("boxplots"),
        width = "100%",
        height = "auto"
      ) %>% withSpinner()) %>%
        bs_append("Summary table",
                  htmlOutput(ns("summary_table"))) %>%
      bs_append("Annotation distribution across clusters",
                  plotOutput(ns("annotation_summary"), height = "auto")
                )
    ) %>% shinyhelper::helper(type = "markdown", content = "boxplots_help")
  )
}

server_boxplots <- function(id, selected_data, cluster_labels, cluster_colors,
                            boxplot_annotation) {
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

    df_long <- reactive({
      validate(need(
        length(input$boxplots_selection) > 0,
        "No clusters selected."
      ))
      isolate({
        c_labels <- cluster_labels()
        s_data <- selected_data()
      })
      req(all(input$boxplots_selection %in% c_labels))
      annotate_clusters(s_data,
                        c_labels,
                        TRUE,
                        input$boxplots_selection)
    })

    boxplots_height <- reactive({
      max(225, ncol(selected_data()) %/% 4 * 225)
    })

    # Calculate boxplots for each cluster, faceting by all_data variables
    output$boxplots <- renderPlot({
      isolate({
        shape <- if (nrow(selected_data()) < 500) "boxplot" else "violin"
      })
      facet_boxplot(
        df_long(), "Cluster", "Value", "Measurement", cluster_colors, shape
      )
    }, height = function() boxplots_height())

    output$annotation_summary <- renderPlot({
      validate(need(ncol(boxplot_annotation()) > 0,
                    "No annotations were selected"))
      plot_annotation_dist(boxplot_annotation(),
                           cluster_labels(),
                           input$boxplots_selection)
    }, height = function() max(200, ncol(boxplot_annotation()) * 150 + 200) )


    # Create table showing summary for unscaled data across clusters
    output$summary_table <- renderText({
      validate(need(
        length(input$boxplots_selection) > 0,
        "No clusters were selected."
      ))
      df_wide <- annotate_clusters(selected_data(),
                                   cluster_labels(),
                                   FALSE,
                                   input$boxplots_selection)
      # Compute IQR and get median, lower and upper
      iqr_fn <- function(x) {
        qt <- stats::quantile(x, c(.25, .5, .75))
        paste0(round(qt[2], 2), " (", round(qt[1], 2), "-", round(qt[3],2), ")")
      }
      # Compute across all variables grouped by clusters
      agg_data <- stats::aggregate(df_wide %>% dplyr::select(-.data$Cluster),
                            by = list(df_wide$Cluster),
                            iqr_fn)
      colnames(agg_data)[1] <- "Cluster"
      t(agg_data) %>%
        knitr::kable(caption = "Unscaled median, Q1 and Q3 per cluster",
                      align = "r",
                     "html") %>%
        kableExtra::kable_styling(full_width = T,
                                  position = "left",
                                  font_size = 12) %>%
        kableExtra::column_spec(1,
                          bold = TRUE,
                          extra_css = "position: sticky; background: #FFF") %>%
        kableExtra::scroll_box(width = "900px")
    })

  })
}
