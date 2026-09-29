# Allocation mode

app <- shinyAppDir(".")
w0  <- "example-data/example_w0.csv"

allocate <- function(seed = 69, k = 3, names = "Vehicle, Low dose, High dose") {
  out <- NULL
  testServer(app, {
    session$setInputs(mode = "allocation", skip_lines = "24", file1 = upload(w0),
                      lesion_low = 3.8, lesion_mid = 8.0, num_groups = k,
                      seed = seed, group_names_allocation = names)
    session$setInputs(process_data = 1)
    out <<- user_groups()$summarized
  })
  out
}

s <- allocate()

check("reads a 24-header Fusion export", nrow(s) == 12)
check("assigns every animal to a group", !anyNA(s$group))
check("produces the requested number of groups", nlevels(s$group) == 3)

# Lesion binning: "high" is everything above the mid boundary. There is no
# separate upper cut-off -- that input was removed because it never applied.
check("lesion levels are ordered by severity",
      identical(levels(s$lesion), c("low", "mid", "high")))
check("low bin respects the low boundary",
      all(s$mean_net_turns[s$lesion == "low"] <= 3.8))
check("mid bin sits between the boundaries",
      all(s$mean_net_turns[s$lesion == "mid"] > 3.8 &
          s$mean_net_turns[s$lesion == "mid"] <= 8.0))
check("high bin is everything above the mid boundary",
      all(s$mean_net_turns[s$lesion == "high"] > 8.0))

# Regression: rows used to sort alphabetically (high, low, mid) because
# arrange() ran before the factor levels were set.
first_group <- s$lesion[s$group == levels(s$group)[1]]
check("rows sort by severity within a group",
      !is.unsorted(as.integer(first_group)))

# Regression: real Fusion exports contain animals with a negative session mean.
# A hard y-axis limit of 0 used to drop them from the figure while leaving them
# in the table.
check("example data includes a negative-mean animal", any(s$mean_net_turns < 0))
testServer(app, {
  session$setInputs(mode = "allocation", skip_lines = "24", file1 = upload(w0),
                    lesion_low = 3.8, lesion_mid = 8.0, num_groups = 3, seed = 69,
                    group_names_allocation = "Vehicle, Low dose, High dose")
  session$setInputs(process_data = 1)
  built <- ggplot2::ggplot_build(plot2_obj())
  check("Fig 2 plots every animal, including negative means",
        sum(!is.na(built$data[[2]]$y)) == nrow(user_groups()$summarized))
})

# Anticlustering should leave the groups comparable
gm <- tapply(s$mean_net_turns, s$group, mean)
check("group means are within 3 net turns of each other",
      diff(range(gm)) < 3, sprintf("range was %.2f", diff(range(gm))))

# Reproducibility
map <- function(x) setNames(as.character(x$group), as.character(x$id))[order(as.character(x$id))]
check("same seed reproduces the same allocation",
      identical(map(allocate(seed = 69)), map(allocate(seed = 69))))
check("a different seed changes the allocation",
      !identical(map(allocate(seed = 69)), map(allocate(seed = 123))))

# Figures and downloads
testServer(app, {
  session$setInputs(mode = "allocation", skip_lines = "24", file1 = upload(w0),
                    lesion_low = 3.8, lesion_mid = 8.0, num_groups = 3, seed = 69,
                    group_names_allocation = "Vehicle, Low dose, High dose")
  session$setInputs(process_data = 1)
  for (p in c("plot1_obj", "plot2_obj", "plot3_obj"))
    check(paste(p, "builds"), inherits(ggplot2::ggplot_build(get(p)()), "ggplot_built"))
  # Regression: Figs 2 and 3 both used to download as "violin_plot_<date>.pdf"
  names <- vapply(c("downloadData1", "downloadData2", "downloadData3"),
                  function(n) basename(output[[n]]), character(1))
  check("all three figures download as valid PDFs",
        all(vapply(c("downloadData1","downloadData2","downloadData3"),
                   function(n) is_pdf(output[[n]]), logical(1))))
  check("the three figures have distinct file names",
        length(unique(names)) == 3)
  check("allocation table downloads", grepl("^allocation_table_", basename(output$download_allocation)))
})
