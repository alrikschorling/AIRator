# Load packages
# Declared explicitly rather than via pacman::p_load() so that dependencies are
# discoverable by renv/rsconnect and nothing is installed at runtime on the
# server. rstatix, ggprism and purrr were declared previously but never used.
library(shiny)          # app framework
library(shinythemes)    # bootswatch themes
library(shinyjs)        # enable/disable controls
library(shinyFeedback)  # inline input validation
library(dplyr)          # data manipulation
library(ggplot2)        # plotting
library(anticlust)      # balanced group allocation

# Set themes
basic_theme <- theme_bw() + 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(linewidth = 0.5),
        axis.ticks.length = unit(.2, "cm"), 
        axis.text.x = element_text(color = "black", family = "sans", size = 12),
        axis.text.y = element_text(color = "black", family = "sans", size = 12),
        axis.title = element_text(size = 12), 
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 12),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12), 
        strip.background = element_rect(color = NA, fill = NA), 
        plot.title = element_text(color = "black", family = "sans", size = 12))

theme_1 <- basic_theme + theme(axis.line = element_line(linewidth = 0.5))


# Custom color palette
pal <- c("#B5B5B5", "#80607E", "#64D1C5", "#D1B856", "#8F845B", "#5C7C78", "#524410", "#470C43")


# UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  tags$head(
    tags$style(HTML("
      .btn-spacing {
        margin-bottom: 5px; margin-right: 5px;
      }
    "))
  ),
  useShinyFeedback(),
  useShinyjs(),
  
  tags$head(
    tags$style(HTML("
    .progress {
      width: 100% !important;  /* Make the progress bar 100% width */
      height: 30px !important;  /* Adjust the height of the progress bar */
    }
    .progress-bar {
      font-size: 14px !important;  /* Increase the font size */
      line-height: 30px !important;  /* Match the line height to the bar height to center text */
      text-align: center !important;  /* Ensure the text is centered horizontally */
    }
  "))
  ),
  
  
  titlePanel("AIRator - amphetamine-induced rotations processor"),
  sidebarLayout(
    sidebarPanel(
      
      
      helpText(HTML("Please upload a raw CSV file directly derived from the",
                    "<a href='https://omnitech-usa.com/product/fusion-software/'>Fusion software</a>.",
                    
                    "The CSV files should be generated using the extended export, 60).<br><br>",
                    
                    "If the data was derived from RotoMax system in Copenhagen, the number of columns to",
                    "skip is 22. Otherwise it should be 24.<br><br>",
                    
                    "The app processes the data in two modes:<br>",
                    "<b>1. Allocation:</b> <br>",
                    "Allocates rats in balanced groups using anticlustering with the following parameters:<br>",
                    "- objective = kplus <br>",
                    "- categories = lesion <br>",
                    "- method = local-maximum <br>",
                    "- repetitions = 100 <br><br>",
                    
                    "You can read more about the anticlustering packages <a href='https://cran.r-project.org/web/packages/anticlust/vignettes/anticlust.html'>here</a>.",
                    
                    "<br><br>",
                    "<b>2. Analysis:</b> <br>",
                    "Analyzes the data, visualizes the results, and generates publication-ready figures.",
                    )),
      
      
      selectInput("mode", "Select Mode:", 
                  choices = c("Allocation" = "allocation", "Analysis" = "analysis")),
      selectInput("skip_lines", "Number of lines to skip:", 
                  choices = c(22, 24), selected = 22),
      fileInput("file1", "Choose CSV file(s)", multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      conditionalPanel(
        condition = "input.mode == 'allocation'",
        numericInput("lesion_low", "Lesion boundary - Low (<= value)", 3.8, step = 0.1),
        numericInput("lesion_mid", "Lesion boundary - Mid (<= value)", 8.0, step = 0.1),
        helpText("Animals above the mid boundary are classified as \"high\"."),
        numericInput("num_groups", "Number of groups for anticlustering", 3),
        numericInput("seed", "Random seed (for reproducibility)", 69),
        textInput("group_names_allocation", "Group names (comma-separated)", 
                  placeholder = "Group A, Group B, Group C"),  # Comma-separated input
        actionButton("process_data", "Process data", class = "btn-spacing"),
        downloadButton("download_allocation", "Download allocation table results", class = "btn-spacing")
        
      ),
      conditionalPanel(
        condition = "input.mode == 'analysis'",
        textInput("group_names", "Group names (comma-separated)", 
                  placeholder = "Group A, Group B, Group C"),
        uiOutput("dynamic_group_ui"),
        uiOutput("week_input_ui"),  # Dynamic UI for week input
        actionButton("process_data_analysis", "Process Data")
      ),
      downloadButton("downloadData1", "Download Fig 1 - continuous data", class = "btn-spacing"),
      downloadButton("downloadData2", "Download Fig 2 - groups", class = "btn-spacing"),
      downloadButton("downloadData3", "Download Fig 3 - overall distribution", class = "btn-spacing"),
      width = 6
      
    ),
    mainPanel(
      tableOutput("data_dimensions"),  # Output dimensions of uploaded files
      uiOutput("allocation_table_ui"),  # UI output for allocation table
      plotOutput("plot1"),
      plotOutput("plot2"),
      plotOutput("plot3")
    )
  )
)

# Updated Server function
server <- function(input, output, session) {
  
  # Function to read and process the CSV files
  read.func <- function(filename, skip_lines) {
    if (file.exists(filename) && file.info(filename)$size > 0) {
      tryCatch({
        df <- read.csv(filename, sep = ",", dec = ".", skip = skip_lines)
        
        # Columns that may or may not be present
        cols_to_remove <- c("ROTOR", "DURATION..s.", "SUBJECT.TYPE", "START.TIME", 
                            "CLOCKWISE.TURNS", "COUNTER.CLOCKWISE.TURNS", 
                            "REASON.REJECTED", "X")
        
        # Only remove columns that are actually present in the data
        cols_to_remove <- intersect(cols_to_remove, colnames(df))
        
        df <- df %>%
          select(-all_of(cols_to_remove)) %>%
          rename(exp = EXPERIMENT, id = SUBJECT.ID, net_turns = NET.TURNS, min = SAMPLE) %>%
          mutate(id = gsub("#", "", id)) %>%
          mutate(id = factor(id))
        
        return(df)
      }, error = function(e) {
        shinyFeedback::showFeedbackDanger(
          inputId = "file1",
          text = paste("Error reading file:", filename, ":", e$message)
        )
        return(NULL)
      })
    } else {
      shinyFeedback::showFeedbackDanger(
        inputId = "file1",
        text = "File does not exist or is empty. Please ensure the file is downloaded and available."
      )
      return(NULL)
    }
  }
  
  
  # Reactive expression to read in the data files
  data_list <- reactive({
    req(input$mode)
    req(input$file1)
    
    #clear any feedback left over from a previous upload
    shinyFeedback::hideFeedback("file1")

    data_files <- lapply(input$file1$datapath, read.func, skip_lines = input$skip_lines)
    names(data_files) <- input$file1$name
    data_files <- Filter(Negate(is.null), data_files)
    
    if (length(data_files) == 0) {
      shinyFeedback::showFeedbackWarning(
        inputId = "file1",
        text = "No valid files were loaded. Please check your file selections."
      )
    }
    
    data_files
  })
  
  # Reactive expression to combine all data frames into one
  combined_data <- reactive({
    req(data_list())
    #bind_rows rather than rbind: it reports a readable error when files have
    #mismatched columns instead of failing on a column-count mismatch
    combined <- bind_rows(data_list())

    if (input$mode == "analysis") {
      files <- data_list()

      #one week label per uploaded file, matched by file name so a file that
      #failed to read does not shift every subsequent label
      weeks <- vapply(seq_along(input$file1$name), function(i) {
        w <- input[[paste0("week_", i)]]
        if (is.null(w) || !nzchar(w)) NA_character_ else as.character(w)
      }, character(1))
      names(weeks) <- input$file1$name
      combined$week <- rep(unname(weeks[names(files)]),
                           vapply(files, nrow, integer(1)))

      #Look each animal's group up through a named vector. unlist() silently
      #drops NULLs for animals whose group is still unset, which leaves the
      #column shorter than the data and then recycles. IDs are sanitised
      #because they are interpolated into Shiny input ids.
      ids <- unique(as.character(combined$id))
      groups <- vapply(ids, function(id) {
        g <- input[[paste0("group_", make.names(id))]]
        if (is.null(g)) NA_character_ else as.character(g)
      }, character(1))
      combined$group <- unname(groups[match(as.character(combined$id), ids)])
    }
    
    combined
  })
  
  # Generate the dynamic UI for group selections and week input in analysis mode
  observe({
    req(input$file1)
    if (input$mode == "analysis") {
      output$dynamic_group_ui <- renderUI({
        ids <- unique(unlist(lapply(data_list(), function(df) as.character(df$id))))
        group_names <- strsplit(input$group_names, ",\\s*")[[1]]
        
        lapply(ids, function(id) {
          #make.names keeps ids containing spaces or punctuation from
          #producing input ids that Shiny cannot bind to
          selectInput(inputId = paste0("group_", make.names(id)),
                      label = paste("Group for", id),
                      choices = group_names,
                      selected = NULL)
        })
      })
      
      output$week_input_ui <- renderUI({
        tagList(
          lapply(seq_along(input$file1$name), function(i) {
            textInput(inputId = paste0("week_", i), 
                      label = paste("Enter week for", input$file1$name[i]), 
                      placeholder = "e.g., w0")
          })
        )
      })
    }
  })
  
  # Summary of what was actually read. A wrong "lines to skip" setting is the
  # most common upload problem, and it shows up here as an obviously wrong row
  # or animal count instead of a confusing error further downstream.
  output$data_dimensions <- renderTable({
    req(input$file1)
    files <- data_list()
    req(length(files) > 0)

    data.frame(
      File    = names(files),
      Rows    = vapply(files, nrow, integer(1)),
      Animals = vapply(files, function(d) length(unique(d$id)), integer(1)),
      row.names = NULL, check.names = FALSE
    )
  })

  # Define function to calculate the standard error of the mean (SEM)
  sem_func <- function(turns) {
    sd(turns) / sqrt(length(turns))
  }
  
  # Reactive expression to generate user groups
  user_groups <- eventReactive(c(input$process_data, input$mode), {
    req(combined_data())
    
    if (input$mode == "allocation") {
      # For Allocation Mode
      
      req(input$seed)
      set.seed(input$seed)  # fixed seed makes an allocation reproducible
      summarized_data <- combined_data() %>%
        group_by(id) %>%
        summarise(mean_net_turns = mean(net_turns), 
                  sem = sem_func(net_turns), 
                  .groups = 'drop') %>%
        # Three ordered bins. "high" is everything above the mid boundary --
        # there is deliberately no separate upper cut-off.
        mutate(lesion = case_when(
          mean_net_turns <= input$lesion_low ~ "low",
          mean_net_turns <= input$lesion_mid ~ "mid",
          TRUE                               ~ "high"
        )) %>%
        mutate(group = factor(anticlustering(
          mean_net_turns,
          K = input$num_groups,
          objective = "kplus",
          categories = lesion,
          method = "local-maximum",
          repetitions = 100
        ))) %>%
        # set the factor levels BEFORE arranging, otherwise rows sort
        # alphabetically (high, low, mid) instead of by severity
        mutate(lesion = factor(lesion, levels = c("low", "mid", "high"))) |>
        arrange(group, lesion)
      
      # Parse the comma-separated group names
      group_names <- strsplit(input$group_names_allocation, ",\\s*")[[1]]
      
      # Ensure the number of group names matches the number of groups
      if (length(group_names) != input$num_groups) {
        shinyFeedback::showFeedbackDanger(
          inputId = "group_names_allocation",
          text = "Group name and number mismatch"
        )
        return(NULL)
      }
      
      # Assign custom group names
      summarized_data$group <- factor(summarized_data$group, 
                                      levels = 1:input$num_groups, 
                                      labels = group_names)
      
      original_data_with_groups <- combined_data() %>%
        left_join(summarized_data %>% select(id, group, mean_net_turns, sem), by = "id")
      
      return(list(summarized = summarized_data, original = original_data_with_groups))
      
    } else if (input$mode == "analysis") {
      # For Analysis Mode
      summarized_data <- combined_data() %>%
        group_by(week, group) %>%
        summarise(mean_net_turns = mean(net_turns), 
                  sem = sem_func(net_turns), 
                  .groups = 'drop')
      
      combined_data_with_groups <- combined_data() %>%
        left_join(summarized_data %>% select(week, group, mean_net_turns, sem), 
                  by = c("week", "group"))
      
      return(list(analysis = combined_data_with_groups))
    }
  })
  
  
  # Conditional UI for showing the allocation table only after processing
  output$allocation_table_ui <- renderUI({
    req(input$process_data)
    tableOutput("allocation_table")
  })
  
  # Render the allocation results table based on summarized data
  output$allocation_table <- renderTable({
    req(user_groups())
    user_groups()$summarized
  })
  
  # ---- Figures -----------------------------------------------------------
  # Each figure is built exactly once here and consumed by both the on-screen
  # output and the PDF download, so a downloaded file can never drift from the
  # figure that was reviewed on screen.

  plot1_obj <- reactive({
    if (input$mode != "allocation") return(NULL)
    original_data <- user_groups()$original
    if (!all(c("min", "net_turns", "id", "group") %in% colnames(original_data))) return(NULL)

    unique_groups <- unique(original_data$group)
    colors <- pal[seq_along(unique_groups)]

    ggplot(original_data, aes(x = min, y = net_turns, color = group)) +
      geom_point(alpha = 0.3, size = 2) +
      geom_smooth(method = "loess", se = FALSE) +
      facet_wrap(~id, scale = "free", drop = FALSE) +
      #breaks every 30 min, but no hard upper limit: a fixed c(0, 90) silently
      #dropped any data from a session longer than 90 minutes
      scale_x_continuous(breaks = seq(0, max(original_data$min, na.rm = TRUE), by = 30)) +
      scale_color_manual(values = setNames(colors, unique_groups)) +
      theme_1 +
      labs(x = "Time (min)", y = "  Net turns \n (per min)",
           title = "Fig 1. Continuous data") +
      theme(legend.position = "bottom")
  })

  plot2_obj <- reactive({
    if (input$mode != "allocation") return(NULL)
    summarized_data <- user_groups()$summarized
    if (!all(c("mean_net_turns", "group") %in% colnames(summarized_data))) return(NULL)

    unique_groups <- unique(summarized_data$group)
    colors <- pal[seq_along(unique_groups)]

    ggplot(summarized_data, aes(x = group, y = mean_net_turns, fill = group)) +
      geom_violin() +
      # alpha is a fixed setting, not a mapping -- keeping it outside aes()
      # avoids a spurious "0.8" legend entry
      geom_point(position = position_jitter(width = 0.2),
                 size = 4, shape = 21, stroke = 0.2,
                 fill = "white", color = "black", alpha = 0.8) +
      scale_y_continuous(expand = c(0, 0),
                         limits = c(0, 1.2 * max(summarized_data$mean_net_turns))) +
      scale_fill_manual(values = setNames(colors, unique_groups)) +
      theme_1 +
      labs(x = "Group", y = "Mean net turns", title = "Fig 2. Allocation groups")
  })

  plot3_obj <- reactive({
    if (input$mode != "allocation") return(NULL)
    summarized_data <- user_groups()$summarized
    if (!all(c("mean_net_turns", "lesion") %in% colnames(summarized_data))) return(NULL)

    ggplot(summarized_data, aes(x = 1, y = mean_net_turns)) +
      geom_violin(fill = "gray90") +
      geom_point(aes(fill = lesion),
                 position = position_jitter(width = 0.2),
                 size = 4, shape = 21, stroke = 0.2, color = "black") +
      scale_y_continuous(expand = c(0, 0),
                         limits = c(0, 1.2 * max(summarized_data$mean_net_turns))) +
      # named so the colours stay attached to the right bin even when a
      # category happens to be empty
      scale_fill_manual(values = c(low = "#DBF227", mid = "#9FC131", high = "#005C53")) +
      geom_hline(yintercept = c(input$lesion_low, input$lesion_mid), linetype = "dashed") +
      theme_1 +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
      labs(x = "", y = "Mean net turns", title = "Fig 3. Lesion results")
  })

  #on-screen figures
  output$plot1 <- renderPlot(plot1_obj())
  output$plot2 <- renderPlot(plot2_obj())
  output$plot3 <- renderPlot(plot3_obj())

  #figure downloads are only meaningful once the data has been processed
  figure_buttons <- c("downloadData1", "downloadData2", "downloadData3")
  lapply(figure_buttons, shinyjs::disable)
  observeEvent(plot1_obj(), lapply(figure_buttons, shinyjs::enable), ignoreNULL = TRUE)

  #build a PDF download handler for a given figure and page size
  pdf_download <- function(prefix, plot_reactive, width, height) {
    downloadHandler(
      filename = function() paste0(prefix, "_", Sys.Date(), ".pdf"),
      content = function(file) {
        p <- isolate(plot_reactive())
        validate(need(!is.null(p),
                      "Nothing to download yet - process the data first."))
        ggsave(file, plot = p, device = "pdf",
               width = width, height = height, units = "in")
      }
    )
  }

  output$downloadData1 <- pdf_download("continuous_data_plot", plot1_obj, 8, 6)
  output$downloadData2 <- pdf_download("allocation_groups",    plot2_obj, 4, 3)
  output$downloadData3 <- pdf_download("lesion_results",       plot3_obj, 3, 3)

  # Download allocation table as CSV
  output$download_allocation <- downloadHandler(
    filename = function() {
      paste("allocation_table_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      summarized_data <- user_groups()$summarized
      
      # Round numeric columns to 2 decimal places and ensure proper formatting
      rounded_data <- summarized_data %>%
        mutate(across(where(is.numeric), ~ format(round(., 2), nsmall = 2)))  # Round and format numeric columns
      
      # Write the rounded and formatted table to a CSV file
      write.csv(rounded_data, file, row.names = FALSE, quote = TRUE)
    }
  )
  
}  

# Run the application
shinyApp(ui = ui, server = server)
