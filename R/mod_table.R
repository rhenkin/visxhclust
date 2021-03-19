ui_table <- function() {
  ns <- NS("table")
  tabPanel(
    "Table view",
    value = "table",
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
        4,
        style = "margin-top: 25px",
        offset = 1,
        actionButton(ns("remove_subjects"), "Remove selected subjects")
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
      selection = "multiple",
      options = list(scrollX = TRUE, scrollCollapse = TRUE)
    )


    new_data <- eventReactive(input$remove_subjects, {
      isolate({
        all_df <- all_data()
      })
      new_data <-
        remove_selected_rows(all_df,
                             cluster_labels(),
                             input$clusters_selection,
                             input$clusters_table_rows_selected)
      # this updates all_data after removing rows
    })

    new_data

  })
}
