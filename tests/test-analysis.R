# Analysis mode

app   <- shinyAppDir(".")
weeks <- file.path("example-data", c("example_w0.csv", "example_w4.csv", "example_w8.csv"))

# The app sanitises subject ids into input ids with make.names(), so the
# numeric id "1" becomes the input "group_X1".
group_inputs <- function(assignment)
  setNames(as.list(assignment), paste0("group_", make.names(as.character(seq_along(assignment)))))

analyse <- function(assignment = ifelse(seq_len(12) %% 2 == 1, "Treated", "Control"),
                    names = "Treated, Control", labels = c("w0", "w4", "w8"),
                    files = weeks, capture = NULL) {
  out <- NULL
  testServer(app, {
    session$setInputs(mode = "analysis", skip_lines = "24",
                      file1 = upload(files), group_names = names)
    do.call(session$setInputs,
            setNames(as.list(labels), paste0("week_", seq_along(labels))))
    do.call(session$setInputs, group_inputs(assignment))
    session$setInputs(process_data_analysis = 1)
    out <<- if (is.null(capture)) user_groups() else capture(environment())
  })
  out
}

res <- analyse()

check("aggregates to one row per week and group", nrow(res$per_group) == 6)
check("keeps every animal", all(res$per_group$n == 6))
check("weeks follow upload order, not alphabetical order",
      identical(levels(res$per_group$week), c("w0", "w4", "w8")))

# Regression: SEM used to be computed over pooled animal x minute rows, which
# treated every minute as an independent observation and understated it roughly
# sevenfold. It must be the SEM across animals.
pa  <- res$per_animal
own <- tapply(pa$mean_net_turns, list(pa$week, pa$group), function(v) sd(v)/sqrt(length(v)))
check("SEM is computed across animals, not across minutes",
      all(abs(sort(res$per_group$sem) - sort(as.vector(own))) < 1e-8))

# Regression: summarise() evaluates in order, so computing sem after
# overwriting mean_net_turns silently produced NA for every group.
check("SEM is not NA", !anyNA(res$per_group$sem))
check("one value per animal per week", nrow(res$per_animal) == 12 * 3)

# The simulated treatment effect should show up as a decline in the treated arm
tr <- res$per_group[res$per_group$group == "Treated", ]
ct <- res$per_group[res$per_group$group == "Control", ]
check("treated arm declines across weeks", tr$mean_net_turns[3] < tr$mean_net_turns[1])
check("control arm does not decline", ct$mean_net_turns[3] > ct$mean_net_turns[1] - 1)

# Per-week statistics
st <- analyse(capture = function(e) get("analysis_stats", envir = e)())
check("one comparison per week with two groups", nrow(st) == 3)
check("weeks are tested independently", length(unique(st$week)) == 3)
check("a two-group comparison uses a t-test", all(grepl("t-test", st$method)))
check("groups are balanced at baseline", st$p[st$week == "w0"] > 0.05)

# Three groups must route to a post-hoc over all pairs
st3 <- analyse(assignment = rep(c("A", "B", "C"), length.out = 12),
               names = "A, B, C",
               capture = function(e) get("analysis_stats", envir = e)())
check("three groups give three comparisons per week", nrow(st3) == 9)
check("three groups use a post-hoc, not a t-test",
      all(grepl("Tukey|Games-Howell|Dunn", st3$method)))

# Guards
check_error("refuses to analyse when a file has no week label",
            analyse(labels = c("w0", "w4")),
            "Every uploaded file needs a week label")
check_error("refuses to analyse when an animal has no group",
            analyse(assignment = c(NA, ifelse(2:12 %% 2 == 1, "Treated", "Control"))),
            "Every animal needs to be assigned to a group")

# Figures and downloads
testServer(app, {
  session$setInputs(mode = "analysis", skip_lines = "24", file1 = upload(weeks),
                    group_names = "Treated, Control")
  session$setInputs(week_1 = "w0", week_2 = "w4", week_3 = "w8")
  do.call(session$setInputs, group_inputs(ifelse(seq_len(12) %% 2 == 1, "Treated", "Control")))
  session$setInputs(process_data_analysis = 1)
  for (p in c("plot1_obj", "plot2_obj", "plot3_obj"))
    check(paste("analysis", p, "builds"),
          inherits(ggplot2::ggplot_build(get(p)()), "ggplot_built"))
  check("analysis figures download as valid PDFs",
        all(vapply(c("downloadData1","downloadData2","downloadData3"),
                   function(n) is_pdf(output[[n]]), logical(1))))
  # file names follow the mode, not the button
  check("analysis figure names differ from allocation names",
        grepl("^session_time_course_", basename(output$downloadData1)))
  check("analysis summary downloads",
        grepl("^analysis_summary_", basename(output$download_analysis)))
})
