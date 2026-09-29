# Load packages
# Declared explicitly rather than via pacman::p_load() so that dependencies are
# discoverable by renv/rsconnect and nothing is installed at runtime on the
# server.
library(shiny)          # app framework
library(shinythemes)    # bootswatch themes
library(shinyjs)        # enable/disable controls
library(shinyFeedback)  # inline input validation
library(dplyr)          # data manipulation
library(ggplot2)        # plotting
library(anticlust)      # balanced group allocation
library(rstatix)        # per-week group comparisons in analysis mode
library(ggprism)        # add_pvalue()

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
                    "Compares treatment groups over time. Upload one file per timepoint, label each",
                    "file with its week and assign every animal to a group.<br>",
                    "Each animal contributes one value per week (its mean net turns over the session),",
                    "so error bars reflect variation between animals rather than between minutes.<br>",
                    "Within each week the groups are compared, with the test chosen from the data:<br>",
                    "- non-normal residuals: Wilcoxon (2 groups) or Dunn, BH-adjusted (3+)<br>",
                    "- unequal variance: Welch (2 groups) or Games-Howell (3+)<br>",
                    "- otherwise: Student t-test (2 groups) or ANOVA + Tukey HSD (3+)",
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
        actionButton("process_data_analysis", "Process data", class = "btn-spacing"),
        downloadButton("download_analysis", "Download analysis summary", class = "btn-spacing")
      ),
      conditionalPanel(
        condition = "input.mode == 'allocation'",
        helpText(HTML("<b>Fig 1</b> continuous data &middot; <b>Fig 2</b> allocation groups",
                      "&middot; <b>Fig 3</b> lesion results"))),
      conditionalPanel(
        condition = "input.mode == 'analysis'",
        helpText(HTML("<b>Fig 1</b> session time course &middot; <b>Fig 2</b> group comparison per week",
                      "&middot; <b>Fig 3</b> response over weeks"))),
      downloadButton("downloadData1", "Download Fig 1", class = "btn-spacing"),
      downloadButton("downloadData2", "Download Fig 2", class = "btn-spacing"),
      downloadButton("downloadData3", "Download Fig 3", class = "btn-spacing"),
      width = 6
      
    ),
    mainPanel(
      tableOutput("data_dimensions"),  # Output dimensions of uploaded files
      uiOutput("allocation_table_ui"),  # UI output for allocation table
      uiOutput("analysis_table_ui"),    # UI output for analysis summary
      verbatimTextOutput("analysis_tests"),
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
    turns <- turns[!is.na(turns)]
    if (length(turns) < 2) return(NA_real_)
    sd(turns) / sqrt(length(turns))
  }

  # Week labels in the order the files were uploaded. Weeks are free text, so
  # sorting them would put "w10" before "w2"; upload order is the chronological
  # order the user intends.
  week_levels <- reactive({
    req(input$file1)
    ws <- vapply(seq_along(input$file1$name), function(i) {
      w <- input[[paste0("week_", i)]]
      if (is.null(w) || !nzchar(w)) NA_character_ else as.character(w)
    }, character(1))
    unique(ws[!is.na(ws)])
  })

  # Group names for analysis mode, in the order the user typed them
  analysis_group_names <- reactive({
    gn <- strsplit(input$group_names %||% "", ",\\s*")[[1]]
    gn[nzchar(gn)]
  })
  
  # Reactive expression to generate user groups
  user_groups <- eventReactive(
    c(input$process_data, input$process_data_analysis, input$mode), {
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
      d <- combined_data()

      validate(
        need(length(analysis_group_names()) >= 1,
             "Enter the group names first (comma-separated)."),
        need(length(week_levels()) >= 1,
             "Give every uploaded file a week label."),
        need(!anyNA(d$week),
             "Every uploaded file needs a week label before the data can be analysed."),
        need(!anyNA(d$group),
             "Every animal needs to be assigned to a group before the data can be analysed.")
      )

      d <- d %>%
        mutate(week  = factor(week,  levels = week_levels()),
               group = factor(group, levels = analysis_group_names()))

      # One value per animal per week: the mean over that animal's session.
      # This is the unit of analysis. Summarising straight from the per-minute
      # rows would treat every minute as an independent observation and
      # understate the SEM several-fold.
      per_animal <- d %>%
        group_by(week, group, id) %>%
        summarise(mean_net_turns = mean(net_turns, na.rm = TRUE), .groups = "drop")

      # Group-level summary, with the SEM taken across ANIMALS
      # NOTE: summarise() evaluates its arguments in order and later ones see
      # the earlier results, so sem must be computed BEFORE mean_net_turns is
      # overwritten -- otherwise sem_func() receives the scalar mean rather
      # than the column, and every SEM comes out NA.
      per_group <- per_animal %>%
        group_by(week, group) %>%
        summarise(n              = dplyr::n(),
                  sem            = sem_func(mean_net_turns),
                  mean_net_turns = mean(mean_net_turns, na.rm = TRUE),
                  .groups = "drop") %>%
        select(week, group, n, mean_net_turns, sem)

      # Group mean at each minute, for the time-course figure. Here each animal
      # does contribute one observation per minute, so the SEM is across animals.
      per_minute <- d %>%
        group_by(week, group, min) %>%
        summarise(mean_net_turns = mean(net_turns, na.rm = TRUE),
                  sem = sem_func(net_turns),
                  .groups = "drop")

      return(list(per_animal = per_animal,
                  per_group  = per_group,
                  per_minute = per_minute))
    }
  })
  
  
  # ---- Per-week group comparison (analysis mode) --------------------------
  # Each week is tested on its own, on the per-animal means. The test is chosen
  # from the data, matching the rule allocatoR uses and extending it to more
  # than two groups.
  analysis_stats <- reactive({
    if (input$mode != "analysis") return(NULL)
    res <- user_groups()
    if (is.null(res$per_animal)) return(NULL)

    d <- droplevels(res$per_animal)
    if (nlevels(d$group) < 2) return(NULL)

    rows <- lapply(levels(d$week), function(w) {
      wk <- droplevels(d[d$week == w, , drop = FALSE])
      # every group needs at least two animals for any of these tests
      if (nlevels(wk$group) < 2 || any(table(wk$group) < 2)) return(NULL)

      # Normality is checked on the within-group residuals, which is the
      # assumption the t-test and ANOVA actually make -- not on the raw values,
      # which are a mixture of the group means.
      resid <- wk$mean_net_turns - ave(wk$mean_net_turns, wk$group, FUN = mean)
      normal <- tryCatch(stats::shapiro.test(resid)$p.value >= 0.05,
                         error = function(e) TRUE)
      equal_var <- tryCatch(levene_test(wk, mean_net_turns ~ group)$p >= 0.05,
                            error = function(e) TRUE)
      two <- nlevels(wk$group) == 2

      tag <- function(result, label) { result$method <- label; result }

      tst <- tryCatch({
        if (!normal && two) {
          tag(wilcox_test(wk, mean_net_turns ~ group), "Wilcoxon rank-sum")
        } else if (!normal) {
          tag(dunn_test(wk, mean_net_turns ~ group, p.adjust.method = "BH"),
              "Dunn (BH-adjusted)")
        } else if (two) {
          tag(t_test(wk, mean_net_turns ~ group, var.equal = equal_var),
              if (equal_var) "Student's t-test" else "Welch's t-test")
        } else if (equal_var) {
          tag(tukey_hsd(wk, mean_net_turns ~ group), "ANOVA + Tukey HSD")
        } else {
          tag(games_howell_test(wk, mean_net_turns ~ group), "Games-Howell")
        }
      }, error = function(e) NULL)
      if (is.null(tst) || nrow(tst) == 0) return(NULL)

      # methods differ in whether they report a raw or an adjusted p
      pval <- if ("p.adj" %in% names(tst)) tst$p.adj else tst$p

      # stack the brackets above the data within this week
      ymax <- max(wk$mean_net_turns, na.rm = TRUE)
      step <- 0.10 * max(ymax, 1)

      data.frame(
        week       = w,
        group1     = as.character(tst$group1),
        group2     = as.character(tst$group2),
        p          = as.numeric(pval),
        method     = as.character(tst$method),
        y.position = ymax + step * seq_len(nrow(tst)),
        stringsAsFactors = FALSE
      )
    })

    out <- bind_rows(rows)
    if (nrow(out) == 0) return(NULL)
    out$week    <- factor(out$week, levels = levels(d$week))
    out$p.label <- ifelse(out$p < 0.001, "<0.001", sprintf("%.3f", out$p))
    out
  })

  # Plain-language report of which test was applied to each week
  output$analysis_tests <- renderText({
    if (input$mode != "analysis") return(invisible(NULL))
    st <- analysis_stats()
    if (is.null(st)) {
      return(paste("No group comparison available. Each week needs at least two",
                   "groups with two or more animals each."))
    }
    per_week <- st[!duplicated(st$week), c("week", "method")]
    paste0(
      "Group comparison within each week\n",
      paste(sprintf("  %-10s %s", as.character(per_week$week), per_week$method),
            collapse = "\n"),
      "\n\nTest chosen per week from the within-group residuals (Shapiro-Wilk)",
      "\nand the variance across groups (Levene)."
    )
  })

  # Conditional UI for showing the allocation table only after processing
  output$allocation_table_ui <- renderUI({
    req(input$mode == "allocation")
    req(input$process_data)
    tableOutput("allocation_table")
  })
  
  # Render the allocation results table based on summarized data
  output$allocation_table <- renderTable({
    req(user_groups())
    user_groups()$summarized
  })

  # Analysis summary: one row per week and group
  output$analysis_table_ui <- renderUI({
    req(input$mode == "analysis")
    req(input$process_data_analysis)
    tableOutput("analysis_table")
  })

  output$analysis_table <- renderTable({
    req(user_groups()$per_group)
    user_groups()$per_group %>%
      mutate(week = as.character(week), group = as.character(group))
  })

  # Download the analysis summary, with the per-week test results appended
  output$download_analysis <- downloadHandler(
    filename = function() paste0("analysis_summary_", Sys.Date(), ".csv"),
    content = function(file) {
      g <- isolate(user_groups()$per_group)
      validate(need(!is.null(g), "Process the data first."))

      summary_rows <- g %>%
        mutate(across(where(is.numeric), ~ round(.x, 3)))

      con <- file(file, open = "w")
      on.exit(close(con), add = TRUE)
      writeLines("# Group summary (mean and SEM across animals)", con)
      utils::write.csv(summary_rows, con, row.names = FALSE, quote = TRUE)

      st <- isolate(analysis_stats())
      if (!is.null(st)) {
        writeLines("", con)
        writeLines("# Per-week group comparison", con)
        utils::write.csv(
          st[, c("week", "group1", "group2", "p", "method")] %>%
            mutate(p = round(p, 5)),
          con, row.names = FALSE, quote = TRUE)
      }
    }
  )
  
  # ---- Figures -----------------------------------------------------------
  # Each figure is built exactly once here and consumed by both the on-screen
  # output and the PDF download, so a downloaded file can never drift from the
  # figure that was reviewed on screen.

  #colours for a set of group levels
  group_colors <- function(groups) setNames(pal[seq_along(groups)], groups)

  plot1_obj <- reactive({
    if (input$mode == "analysis") {
      d <- user_groups()$per_minute
      if (is.null(d)) return(NULL)
      groups <- levels(d$group)

      return(
        ggplot(d, aes(x = min, y = mean_net_turns,
                      color = group, fill = group)) +
          geom_ribbon(aes(ymin = mean_net_turns - sem, ymax = mean_net_turns + sem),
                      alpha = 0.2, color = NA) +
          geom_line(linewidth = 0.6) +
          facet_wrap(~week) +
          scale_x_continuous(breaks = seq(0, max(d$min, na.rm = TRUE), by = 30)) +
          scale_color_manual(values = group_colors(groups)) +
          scale_fill_manual(values = group_colors(groups)) +
          theme_1 +
          labs(x = "Time (min)", y = "  Net turns \n (per min)",
               title = "Fig 1. Session time course (mean \u00b1 SEM across animals)") +
          theme(legend.position = "bottom")
      )
    }

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
    if (input$mode == "analysis") {
      d <- user_groups()$per_animal
      if (is.null(d)) return(NULL)
      groups <- levels(droplevels(d$group))

      p <- ggplot(d, aes(x = group, y = mean_net_turns)) +
        geom_violin(aes(fill = group), color = "black") +
        geom_point(position = position_jitter(width = 0.15),
                   size = 2.5, shape = 21, stroke = 0.2,
                   fill = "white", color = "black") +
        #free_y would hide between-week differences, which are the point of
        #this figure, so the weeks deliberately share one scale
        facet_wrap(~week) +
        scale_fill_manual(values = group_colors(groups)) +
        scale_y_continuous(expand = expansion(mult = c(0.05, 0.18))) +
        theme_1 +
        labs(x = "Group", y = "Mean net turns",
             title = "Fig 2. Group comparison within each week") +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
              legend.position = "none")

      st <- analysis_stats()
      if (!is.null(st)) {
        p <- p + add_pvalue(st, label = "p.label",
                            bracket.size = 0.4, label.size = 3)
      }
      return(p)
    }

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
    if (input$mode == "analysis") {
      g  <- user_groups()$per_group
      pa <- user_groups()$per_animal
      if (is.null(g)) return(NULL)
      groups <- levels(droplevels(g$group))

      return(
        ggplot(g, aes(x = week, y = mean_net_turns,
                      color = group, group = group)) +
          #faint per-animal trajectories behind the group means, so individual
          #responders are visible rather than averaged away
          geom_line(data = pa, aes(group = id), alpha = 0.25, linewidth = 0.3) +
          geom_errorbar(aes(ymin = mean_net_turns - sem, ymax = mean_net_turns + sem),
                        width = 0.12, linewidth = 0.5) +
          geom_line(linewidth = 0.8) +
          geom_point(size = 3) +
          scale_color_manual(values = group_colors(groups)) +
          theme_1 +
          labs(x = "Week", y = "Mean net turns",
               title = "Fig 3. Response over weeks (mean \u00b1 SEM across animals)") +
          theme(legend.position = "bottom")
      )
    }

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

  #the figures mean different things in the two modes, so the exported file
  #name and page size follow the mode rather than the button
  fig_meta <- function(index) {
    if (input$mode == "analysis") {
      list(prefix = c("session_time_course", "group_comparison",
                      "response_over_weeks")[index],
           width  = c(9, 8, 6)[index],
           height = c(5, 5, 4)[index])
    } else {
      list(prefix = c("continuous_data_plot", "allocation_groups",
                      "lesion_results")[index],
           width  = c(8, 4, 3)[index],
           height = c(6, 3, 3)[index])
    }
  }

  #build a PDF download handler for a given figure
  pdf_download <- function(index, plot_reactive) {
    downloadHandler(
      filename = function() paste0(fig_meta(index)$prefix, "_", Sys.Date(), ".pdf"),
      content = function(file) {
        p <- isolate(plot_reactive())
        validate(need(!is.null(p),
                      "Nothing to download yet - process the data first."))
        m <- isolate(fig_meta(index))
        ggsave(file, plot = p, device = "pdf",
               width = m$width, height = m$height, units = "in")
      }
    )
  }

  output$downloadData1 <- pdf_download(1, plot1_obj)
  output$downloadData2 <- pdf_download(2, plot2_obj)
  output$downloadData3 <- pdf_download(3, plot3_obj)

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
