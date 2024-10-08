# Load packages
pacman::p_load(shiny, shinythemes, shinyjs, shinyFeedback,
               tidyverse, ggplot2, 
               rstatix, ggprism, 
               purrr, anticlust)

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
        numericInput("lesion_high", "Lesion boundary - High (> value)", 12.0, step = 0.1),
        numericInput("num_groups", "Number of groups for anticlustering", 3),
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
          inputId = ifelse(input$mode == "allocation", "file1", "file1_analysis"),
          text = paste("Error reading file:", filename, ":", e$message)
        )
        return(NULL)
      })
    } else {
      shinyFeedback::showFeedbackDanger(
        inputId = ifelse(input$mode == "allocation", "file1", "file1_analysis"),
        text = "File does not exist or is empty. Please ensure the file is downloaded and available."
      )
      return(NULL)
    }
  }
  
  
  # Reactive expression to read in the data files
  data_list <- reactive({
    req(input$mode)
    req(input$file1)
    
    data_files <- lapply(input$file1$datapath, read.func, skip_lines = input$skip_lines)
    data_files <- Filter(Negate(is.null), data_files)
    
    if (length(data_files) == 0) {
      shinyFeedback::showFeedbackWarning(
        inputId = ifelse(input$mode == "allocation", "file1", "file1_analysis"),
        text = "No valid files were loaded. Please check your file selections."
      )
    }
    
    data_files
  })
  
  # Reactive expression to combine all data frames into one
  combined_data <- reactive({
    req(data_list())
    combined <- do.call(rbind, data_list())
    
    if (input$mode == "analysis") {
      combined$week <- unlist(lapply(seq_along(data_list()), function(i) {
        rep(input[[paste0("week_", i)]], nrow(data_list()[[i]]))
      }))
      
      # Ensure group column exists
      combined$group <- unlist(lapply(combined$id, function(id) {
        input[[paste0("group_", id)]]
      }))
    }
    
    combined
  })
  
  # Generate the dynamic UI for group selections and week input in analysis mode
  observe({
    req(input$file1)
    if (input$mode == "analysis") {
      output$dynamic_group_ui <- renderUI({
        ids <- unique(unlist(lapply(data_list(), function(df) df$id)))
        group_names <- strsplit(input$group_names, ",\\s*")[[1]]
        
        lapply(ids, function(id) {
          selectInput(inputId = paste0("group_", id),
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
  
  # Define function to calculate the standard error of the mean (SEM)
  sem_func <- function(turns) {
    sd(turns) / sqrt(length(turns))
  }
  
  # Reactive expression to generate user groups
  user_groups <- eventReactive(c(input$process_data, input$mode), {
    req(combined_data())
    
    if (input$mode == "allocation") {
      # For Allocation Mode
      
      set.seed(69)  # Set seed for reproducibility
      summarized_data <- combined_data() %>%
        group_by(id) %>%
        summarise(mean_net_turns = mean(net_turns), 
                  sem = sem_func(net_turns), 
                  .groups = 'drop') %>%
        mutate(lesion = case_when(
          mean_net_turns <= input$lesion_low ~ "low",
          mean_net_turns <= input$lesion_mid ~ "mid",
          mean_net_turns > input$lesion_mid ~ "high"
        )) %>%
        mutate(group = factor(anticlustering(
          mean_net_turns,
          K = input$num_groups,
          objective = "kplus",
          categories = lesion,
          method = "local-maximum",
          repetitions = 100
        ))) %>%
        arrange(group, lesion) |>
        mutate(lesion = factor(lesion, levels = c("low", "mid", "high")))
      
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
  
  # Debugging output to check structure of user_groups
  output$debug_user_groups <- renderPrint({
    req(user_groups())
    str(user_groups())
  })
  
  
  
  # Display plot1 in the UI
  output$plot1 <- renderPlot({
    req(user_groups())  # Make sure the data is ready
    data_list <- user_groups()
    
    if (input$mode == "allocation") {
      original_data <- data_list$original
      
      if (all(c("min", "net_turns", "id", "group") %in% colnames(original_data))) {
        unique_groups <- unique(original_data$group)
        colors <- pal[1:length(unique_groups)]
        
        p1 <- ggplot(original_data, aes(x = min, y = net_turns, color = group)) +
          geom_point(alpha = 0.3, size = 2) +
          geom_smooth(method = 'loess', se = FALSE) +
          facet_wrap(~id, scale = 'free', drop = FALSE) +
          scale_x_continuous(limit = c(0, 90), breaks = c(0, 30, 60, 90)) +
          scale_color_manual(values = setNames(colors, unique_groups)) +
          theme_1 +
          labs(x = "Time (min)", y = "  Net turns \n (per min)", title = "Fig 1. Continuous data") +
          theme(legend.position = "bottom")
        
        print(p1)
      }
    }
  })
  
  # Download plot1 as PDF
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste("continuous_data_plot_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file, width = 8, height = 6)  # Open PDF device with size
      plot1 <- isolate(user_groups())  # Fetch the data
      data_list <- plot1
      
      if (input$mode == "allocation") {
        original_data <- data_list$original
        
        if (all(c("min", "net_turns", "id", "group") %in% colnames(original_data))) {
          unique_groups <- unique(original_data$group)
          colors <- pal[1:length(unique_groups)]
          
          p1 <- ggplot(original_data, aes(x = min, y = net_turns, color = group)) +
            geom_point(alpha = 0.3, size = 2) +
            geom_smooth(method = 'loess', se = FALSE) +
            facet_wrap(~id, scale = 'free', drop = FALSE) +
            scale_x_continuous(limit = c(0, 90), breaks = c(0, 30, 60, 90)) +
            scale_color_manual(values = setNames(colors, unique_groups)) +
            theme_1 +
            labs(x = "Time (min)", y = "  Net turns \n (per min)", title = "Fig 1. Continuous data") +
            theme(legend.position = "bottom")
          
          print(p1)
        }
      }
      
      dev.off()  # Close PDF device
    }
  )
  
  
  # Display plot2 in the UI
  output$plot2 <- renderPlot({
    req(user_groups())  # Make sure the data is ready
    data_list <- user_groups()
    
    if (input$mode == "allocation") {
      summarized_data <- data_list$summarized
      
      if (all(c("mean_net_turns", "group") %in% colnames(summarized_data))) {
        unique_groups <- unique(summarized_data$group)
        colors <- pal[1:length(unique_groups)]
        
        p2 <- ggplot(summarized_data, aes(x = group, y = mean_net_turns, fill = group)) +
          geom_violin() +
          geom_point(aes(alpha = 0.8),
                     position = position_jitter(width = 0.2),
                     size = 4, shape = 21, stroke = 0.2, fill = 'white', color = 'black') +
          scale_y_continuous(expand = c(0, 0), limits = c(0, 1.2*max(summarized_data$mean_net_turns))) +
          scale_fill_manual(values = setNames(colors, unique_groups)) +
          theme_1 +
          labs(x = "Group", y = "Mean net turns", title = "Fig 2. Allocation groups")
        
        print(p2)
      }
    }
  })
  
  # Download plot2 as PDF
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste("violin_plot_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file, width = 4, height = 3)  # Open PDF device with size
      plot2 <- isolate(user_groups())  # Fetch the data
      data_list <- plot2
      
      if (input$mode == "allocation") {
        summarized_data <- data_list$summarized
        
        if (all(c("mean_net_turns", "group") %in% colnames(summarized_data))) {
          unique_groups <- unique(summarized_data$group)
          colors <- pal[1:length(unique_groups)]
          
          p2 <- ggplot(summarized_data, aes(x = group, y = mean_net_turns, fill = group)) +
            geom_violin() +
            geom_point(aes(alpha = 0.8),
                       position = position_jitter(width = 0.2),
                       size = 4, shape = 21, stroke = 0.2, fill = 'white', color = 'black') +
            scale_y_continuous(expand = c(0, 0)) +
            scale_fill_manual(values = setNames(colors, unique_groups)) +
            theme_1 +
            labs(x = "Group", y = "Mean net turns", title = "Fig 2. Allocation groups")
          
          print(p2)
        }
      }
      
      dev.off()  # Close PDF device
    }
  )
  
 
  
  
  
  
  #newly added plot 3
  # Display plot3 in the UI
  output$plot3 <- renderPlot({
    req(user_groups())  # Make sure the data is ready
    data_list <- user_groups()
    
    if (input$mode == "allocation") {
      summarized_data <- data_list$summarized
      
      if (all(c("mean_net_turns", "group") %in% colnames(summarized_data))) {
        
        p3 <- ggplot(summarized_data, aes(x = 1, y = mean_net_turns)) +
          geom_violin(fill = 'gray90') +
          geom_point(aes(fill = lesion),
                     position = position_jitter(width = 0.2),
                     size = 4, shape = 21, stroke = 0.2, color = 'black') +
          scale_y_continuous(expand = c(0, 0), limits = c(0, 1.2*max(summarized_data$mean_net_turns))) +
          scale_fill_manual(values = c('#DBF227', '#9FC131', '#005C53')) +
          geom_hline(yintercept = c(input$lesion_low, input$lesion_mid, input$lesion_high), linetype = "dashed") +
          theme_1 +
          theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
          labs(x = "", y = "Mean net turns", title = "Fig 3. Lesion results")
        
        print(p3)
      }
    }
  })
  
  # Download plot3 as PDF
  output$downloadData3 <- downloadHandler(
    filename = function() {
      paste("violin_plot_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file, width = 3, height = 3)  # Open PDF device with size
      plot2 <- isolate(user_groups())  # Fetch the data
      data_list <- plot2
      
      if (input$mode == "allocation") {
        summarized_data <- data_list$summarized
        
        if (all(c("mean_net_turns", "group") %in% colnames(summarized_data))) {
          unique_groups <- unique(summarized_data$group)
          colors <- pal[1:length(unique_groups)]
          
          p3 <- ggplot(summarized_data, aes(x = 1, y = mean_net_turns)) +
            geom_violin(fill = 'gray90') +
            geom_point(aes(fill = lesion),
                       position = position_jitter(width = 0.2),
                       size = 4, shape = 21, stroke = 0.2, color = 'black') +
            scale_y_continuous(expand = c(0, 0), limits = c(0, 1.2*max(summarized_data$mean_net_turns))) +
            theme_1 +
            scale_fill_manual(values = c('#DBF227', '#9FC131', '#005C53')) +
            geom_hline(yintercept = c(input$lesion_low, input$lesion_mid, input$lesion_high), linetype = "dashed") +
            theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
            labs(x = "", y = "Mean net turns", title = "Fig 3. Lesion results")
          
          print(p3)
        }
      }
      
      dev.off()  # Close PDF device
    }
  )
  
  
  
  
  
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
