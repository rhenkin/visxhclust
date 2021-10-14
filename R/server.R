utils::globalVariables(c("where"))
#' Server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @noRd
app_server <- function(input, output, session) {



  shinyhelper::observe_helpers(help_dir = system.file("helpfiles",
                                              package = utils::packageName()))
  session_values <- reactiveValues(numeric_vars = list(),
                                   annotation_vars = list())
  dropped_variables <- reactiveVal(NULL)
  all_data <- reactiveVal(NULL)

  # File loading ----
  loaded_data <- eventReactive(input$file1, {
    file <- input$file1
    load_data(file$datapath)
  })

  # Data and UI initialisation ----
  observeEvent(loaded_data(), {

    new_data <- loaded_data()
    # Check if there is a column with ID
    idColExists <- "ID" %in% names(new_data)
    if (!idColExists) {
      showNotification("Using rownames as ID column.",
                       type = "warning")
      new_data$ID <- rownames(new_data)
    }
    # Convert any character to factor, except ID
    new_data <- dplyr::mutate(new_data,
        dplyr::across(!dplyr::any_of("ID") & where(is.character), as.factor))

    # Find columns with missing values (i.e. NA)
    cols_with_na <- names(which(sapply(new_data, anyNA)))
    anyColumnsNA <- length(cols_with_na) > 0
    if (anyColumnsNA) {
      showNotification("Columns with missing values were found and marked as annotation-only.",
                       type = "warning",
                       duration = NULL)
    }

    numeric_vars <- names(dplyr::select(new_data,
                                        where(is.numeric), -cols_with_na))
    annotation_vars <- names(dplyr::select(new_data,
                                    -.data$ID, -where(is.numeric), cols_with_na))

    # Go through removal of strongly correlated variables
    vars_to_remove <- get_correlated_variables(new_data[, numeric_vars], 0.9)
    dropped_variables(vars_to_remove)
    subset_numeric_vars <- !numeric_vars %in% vars_to_remove[["Var2"]]

    # Update main UI lists (variable selection, overview and comparison tabs)
    updateCheckboxGroupInput(session,
                             "selected_numeric",
                             choices = numeric_vars,
                             selected = numeric_vars[subset_numeric_vars])
    updateCheckboxGroupInput(session,
                             "selected_annotation",
                             choices = annotation_vars)

    # Keep variables in reactive list
    session_values$numeric_vars <- numeric_vars
    session_values$annotation_vars <- annotation_vars

    # Update reactive all_data with the data that was just loaded
    all_data(new_data)

  })

  # Update UI ----
  selected_numeric <- reactive({
    validate(need(!is.null(input$selected_numeric),
                  "No features have been selected"))
    input$selected_numeric
  })

  unselected_vars <- reactive({
    numeric_vars <- session_values$numeric_vars
    lv <- !(numeric_vars %in% selected_numeric())
    numeric_vars[lv]
  })

  observeEvent(input$flip_numeric, {
    numeric_vars <- session_values$numeric_vars
    lv <- !(numeric_vars %in% input$selected_numeric)
    updateCheckboxGroupInput(session,
                             "selected_numeric",
                             selected = numeric_vars[lv])
  })

  observeEvent(input$select_all, {
    if (input$select_all %% 2 == 0) {
      updateCheckboxGroupInput(session,
                               "selected_numeric",
                               choices = session_values$numeric_vars)
    } else {
      updateCheckboxGroupInput(session,
                               "selected_numeric",
                               choices = session_values$numeric_vars,
                               selected = session_values$numeric_vars)
    }
  }, ignoreNULL = TRUE)

  # Selecting data and clustering ----
  # When user changes numeric or categorical selection, triggers a chain reaction
  # Any visible output that depends on selected_data() will be updated
  selected_data <- reactive({
    validate(need(!is.null(all_data()), "No data has been loaded"))
    validCondition <- all(selected_numeric() %in% session_values$numeric_vars)
    validate(need(validCondition, "Updating"))
    isolate({ all_df <- all_data() })
    all_df[, selected_numeric()]
  })

  scaled_data <- reactive({
    if (input$distance_method != "Binary") {
      scaled_mat <- scale_data(selected_data(), input$scaling)
      isolate({ rownames(scaled_mat) <- all_data()$ID })
    } else scaled_mat <- selected_data()
    scaled_mat
  })

  unselected_data <- reactive({
    dplyr::select(all_data(),
                  where(is.numeric),
                  -input$selected_numeric,
                  -session_values$annotation_vars)
  })

  scaled_unselected_data <- reactive({
    if (ncol(unselected_data()) > 0) {
      scaled_mat <- scale_data(unselected_data(), input$scaling)
      isolate({ rownames(scaled_mat) <- all_data()$ID })
      scaled_mat
    } else {
      NULL
    }
  })

  observeEvent(session_values$numeric_vars, {
    updateNumericInput(session, "corr_threshold", value =  0.9)
  })

  # If the correlation threshold changes, recompute all variables
  # Drop any new variables and also update the select input
  observeEvent(input$corr_threshold, {
    req(all_data())
    numeric_vars <- session_values$numeric_vars
    clinical_numeric_df <- all_data()[, numeric_vars]
    vars_to_remove <- get_correlated_variables(clinical_numeric_df,
                                             input$corr_threshold)
    dropped_variables(vars_to_remove)
    lv_num_vars <- !(numeric_vars %in% vars_to_remove[["Var2"]])
    updateSelectInput(session,
                      "selected_numeric",
                      selected = numeric_vars[lv_num_vars])
  })

  # Reactive computation of distance matrix
  distance_matrix <- reactive({
    req(selected_data())
    req(selected_numeric())
    compute_dmat(selected_data(),
                 input$distance_method,
                 input$scaling,
                 selected_numeric())
  })

  # Reactive computation of clusters
  clusters <- reactive({
    compute_clusters(distance_matrix(), linkage_method = input$linkage_method)
  })

  # Reactive extraction of cluster labels
  cluster_labels <- reactive({
    cut_clusters(clusters(), k = input$nclusters)
  })

  all_data_with_clusters <- reactive({
    all_df  <- all_data()
    cbind(all_df, cluster_labels())
  })

  # Download all_data with cluster annotations
  output$download_clusters <- downloadHandler(
    filename = function() {
      paste(input$distance_method,input$linkage_method,input$nclusters,"clusters.csv", sep = "_")
    },
    content = function(file) {
      utils::write.csv(all_data_with_clusters(), file, row.names = FALSE)
    }
  )
  # end clustering methods

  # Overview tab ----
  server_overview(
    "overview",
    selected_data,
    selected_numeric,
    dropped_variables,
    reactive(input$scaling)
  )

  # Distance matrix tab ----
  server_distancematrix(
    "distancematrix",
    distance_matrix,
    cluster_labels,
    cluster_colors
  )

  # PCA tab ----
  server_pca(
    "pca",
    selected_data,
    cluster_labels,
    cluster_colors
  )

  # Heatmap tab ----
  heatmap_annotation <- reactive({
    if (isTruthy(input$selected_annotation)) {
      create_annotations(all_data(), input$selected_annotation)
    } else {
      NULL
    }
  })

  server_heatmap(
    "heatmap",
    heatmap_annotation,
    clusters,
    reactive(input$nclusters),
    cluster_colors,
    scaled_data,
    scaled_unselected_data,
    reactive({ input$scaling == "None" }),
    reactive(input$distance_method)
  )

  # Boxplots tab ----
  boxplot_annotation <- reactive({
    if (isTruthy(input$selected_annotation)) {
      all_data()[input$selected_annotation]
    } else {
      NULL
    }
  })

  server_boxplots(
    "boxplots",
    selected_data,
    cluster_labels,
    cluster_colors,
    boxplot_annotation
  )

  # Cluster table tab ----
  new_data <- server_table(
                "table",
                all_data,
                selected_data,
                cluster_labels,
                reactive(input$nclusters)
  )
  # Update dataset when there is interaction in the table
  observeEvent(new_data(), {
    all_data(new_data())
  })

  # Significance testing tab ----
  server_significance(
    "signif",
    all_data,
    cluster_labels,
    cluster_colors,
    unselected_vars
  )

  # Evaluation tab ----
  server_evaluation(
    "evaluation",
    selected_data,
    clusters
  )

  # Gap statistic tab ----
  server_gapstat(
    "gapstat",
    selected_data,
    clusters
  )

  # Parameter state management tab ----
  new_params <- server_params(
                  "params",
                  reactive(input$distance_method),
                  reactive(input$linkage_method),
                  reactive(input$nclusters)
  )

  # Observe when a user loads previous settings
  observeEvent(new_params(), {
    conf <- new_params()
    for (x in names(conf)) {
        value_conf <- conf[[x]]
        if (is.numeric(value_conf)) {
          updateNumericInput(session, x, value = value_conf)
        }
        if (is.character(value_conf)) {
          updateSelectInput(session, x , selected = value_conf)
        }
      }
  })
  #end state management
}
