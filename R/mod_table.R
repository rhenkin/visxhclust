ui_table <- function() {
  ns <- NS("table")
  tabPanel(
    "Table view",
    value = "table",
    h5("Choose clusters to filter out rows or keep only one cluster.
       Clusters will be recomputed automatically after clicking on one
       of the buttons"),
    fluidRow(
      column(
        4,
        selectizeInput(
          ns("clusters_selection"),
          "Select cluster:",
          choices = NULL,
          options = list(
            onInitialize = I('function(){this.setValue("");}')
          )
        )
      ),
      column(
        8,
        style = "margin-top: 25px",
        offset = 0,
        flowLayout(
        actionButton(ns("remove_rows"), "Remove selected rows"),
        actionButton(ns("keep_subjects"), "Keep only this cluster"))
      )
    ) %>% shinyhelper::helper(type = "markdown", content = "table_help"),
    fluidRow(DT::DTOutput(ns("clusters_table")))
  )
}

server_table <- function(id, all_data, selected_data, cluster_labels, nclusters) {
  moduleServer(id, function(input, output, session) {
    observeEvent(c(nclusters(), selected_data()), {
      req(selected_data())
      updateSelectInput(session,
        inputId = "clusters_selection",
        choices = 1:nclusters()
      )
    })

    # Update cluster table based on selected cluster
    output$clusters_table <- DT::renderDT({
        req(input$clusters_selection)
        isolate({
          all_df <- all_data()
        })
        clusters <- cluster_labels()
        annotate_clusters(all_df,
                          clusters,
                          long = FALSE,
                          input$clusters_selection)
      },
      caption = "Click on rows to select",
      selection = "multiple",
      options = list(scrollX = TRUE, scrollCollapse = TRUE)
    )

    new_data <- reactiveVal(NULL)
    observeEvent(input$remove_rows, {
      isolate({
        all_df <- all_data()
      })
      updated_data <- remove_selected_rows(all_df,
                                       cluster_labels(),
                                       input$clusters_selection,
                                       input$clusters_table_rows_selected)
      new_data(updated_data)

    })

    observeEvent(input$keep_subjects, {
      isolate({
        all_df <- all_data()
      })
      updated_data <- keep_selected_rows(all_df,
                                         cluster_labels(),
                                         input$clusters_selection)
      new_data(updated_data)

    })

    new_data

  })
}
