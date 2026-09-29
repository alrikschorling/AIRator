# Generate the synthetic example Fusion exports in example-data/.
#
# The files mimic the structure and the statistical behaviour of a real
# Omnitech Fusion "comprehensive output" export, but every value is simulated.
# No real experimental data is included.
#
# Structure copied from a real export:
#   - 24 header lines, so the column header is line 25 (the app's "skip 24")
#   - a trailing comma on every row, which read.csv turns into a column "X"
#   - 90 one-minute samples per animal
#
# Behaviour copied from a real export:
#   - net turns are quantised to quarter turns (the 90 degree threshold)
#   - the response rises over roughly the first hour and then plateaus
#   - successive minutes are strongly autocorrelated (lag-1 r of about 0.89)
#   - within-animal SD scales with that animal's plateau level
#   - one animal rotates the other way and has a negative session mean
#
# Run with:  Rscript data-raw/make_example_data.R

set.seed(2024)

out_dir  <- "example-data"
n_min    <- 90     # samples per animal
n_animal <- 12
rho      <- 0.85   # lag-1 autocorrelation within an animal
tau      <- 22     # onset time constant, in minutes

# Plateau level per animal, spread so that the default lesion boundaries
# (3.8 and 8.0) put animals in all three bins. Two animals rotate the
# other way, which is what produces a negative mean.
# The session mean lands at roughly 0.63 x plateau, because of the onset curve.
# These values are chosen so the default boundaries split the animals 4/4/4.
plateau <- c(-2.5, 1.0, 3.0, 4.8, 7.1, 8.7, 10.3, 11.9, 14.3, 17.5, 20.6, 23.8)
stopifnot(length(plateau) == n_animal)

# Treated animals improve over the weeks; controls do not. The treated/control
# split alternates along the sorted plateau values, so the two groups start out
# balanced -- which is what allocation mode would have produced.
treated <- c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
weeks   <- c(w0 = 0, w4 = 4, w8 = 8)

quarter_turns <- function(x) round(x * 4) / 4

simulate_animal <- function(level) {
  onset <- 1 - exp(-seq_len(n_min) / tau)      # saturating rise
  sd_i  <- max(0.35 * abs(level), 0.4)          # spread scales with level
  # AR(1) noise, scaled so the marginal SD is sd_i
  e <- numeric(n_min)
  e[1] <- rnorm(1, 0, sd_i)
  for (i in 2:n_min) e[i] <- rho * e[i - 1] + rnorm(1, 0, sd_i * sqrt(1 - rho^2))
  quarter_turns(level * onset + e)
}

fusion_header <- function(experiment) {
  c("VENDOR INFORMATION,",
    "Omnitech Electronics Inc.,",
    "5090 Trabue Road,",
    "Columbus Ohio 43228,",
    "www.Omnitech-Electronics.com,",
    "",
    "VARIABLE NAME,VARIABLE DESCRIPTION,",
    "Experiment,The name of the relevant Experiment (this value is user created at acquisition time),",
    "Rotor,The name of the relevant Rotor,",
    "Subject ID,The name of the relevant Subject ID (this value is user created at acquisition time),",
    "Subject Type,The relevant Subject Type (this value is user created at acquisition time),",
    "Sample,The name of the relevant Sample,",
    "Start Time,Represents the time at which the experiment began (as expressed in the time zone in which the experiment was created).,",
    "Duration,Represents time elapsed since the start of the experiment.,",
    "Clockwise Turns,The number of complete clockwise turns traveled by the subject attached to a rotor.,",
    "Counter-Clockwise Turns,The number of complete counter-clockwise turns traveled by the subject attached to a rotor.,",
    "Net Turns,The number of complete clockwise turns minus the number of complete counter-clockwise turns traveled by the subject attached to a rotor.,",
    "",
    "EXPERIMENT NAME,CREATION DATE/TIME,USER NAME,COMMENT,DESCRIPTION,PHASE COUNT,BATCH COUNT,TOTAL RECORDINGS COUNT,ORDER BY BATCH,SUBJECT AGE UNIT,",
    paste0(experiment, ",1/1/2024 10:00:00 AM,Example,Synthetic example data,Simulated rats,1,1,1,No,Months,"),
    "",
    "EXPORT PRECISION,DISTANCE UNIT,SPEED UNIT,TIME UNIT,WEIGHT UNIT,SAMPLE DURATION (s),TURN ANGLE THRESHOLD,",
    "2,Centimeter,CentimetersPerSecond,Second,Gram,60,90° (1/4 turn),",
    "",
    "EXPERIMENT,ROTOR,SUBJECT ID,SUBJECT TYPE,SAMPLE,START TIME,DURATION (s),CLOCKWISE TURNS,COUNTER-CLOCKWISE TURNS,NET TURNS,REASON REJECTED,")
}

dir.create(out_dir, showWarnings = FALSE)

for (wk in names(weeks)) {
  experiment <- paste0("EXAMPLE_AIR_", wk)
  # treated animals lose roughly 15% of their response per week, a large
  # effect chosen so the example clearly demonstrates the per-week comparison
  effect <- ifelse(treated, (1 - 0.15)^weeks[[wk]], 1)

  rows <- character(0)
  for (i in seq_len(n_animal)) {
    turns <- simulate_animal(plateau[i] * effect[i])
    cw    <- quarter_turns(pmax(turns, 0))          # clockwise component
    ccw   <- quarter_turns(pmax(-turns, 0))         # counter-clockwise component
    start <- as.POSIXct("2024-01-01 10:00:00", tz = "UTC") + (seq_len(n_min) - 1) * 60
    rows  <- c(rows, sprintf(
      "%s,Rotor %d,%d,,%d,%s,60.00,%.2f,%.2f,%.2f,,",
      experiment, i, i, seq_len(n_min),
      format(start, "%I:%M:%S %p"), cw, ccw, turns))
  }

  f <- file.path(out_dir, paste0("example_", wk, ".csv"))
  writeLines(c(fusion_header(experiment), rows), f)
  cat("wrote", f, "-", length(rows), "rows\n")
}
