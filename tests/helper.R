# Minimal test harness.
#
# Deliberately dependency-free: the only thing these tests need beyond the app
# itself is shiny, which the app already requires. Using testthat would pull a
# test-only package into renv.lock and therefore into the shinyapps.io deploy.
#
# Run from the repository root with:  Rscript tests/run-tests.R

suppressMessages(library(shiny))

.results <- new.env(parent = emptyenv())
.results$pass <- 0L
.results$fail <- 0L
.results$messages <- character(0)

check <- function(label, condition, detail = NULL) {
  ok <- isTRUE(tryCatch(condition, error = function(e) {
    detail <<- conditionMessage(e); FALSE
  }))
  if (ok) {
    .results$pass <- .results$pass + 1L
    cat(sprintf("  ok   %s\n", label))
  } else {
    .results$fail <- .results$fail + 1L
    msg <- sprintf("  FAIL %s%s", label,
                   if (is.null(detail)) "" else paste0("  (", detail, ")"))
    .results$messages <- c(.results$messages, msg)
    cat(msg, "\n", sep = "")
  }
  invisible(ok)
}

# Assert that an expression stops, and that the message matches a pattern.
check_error <- function(label, expr, pattern) {
  msg <- tryCatch({ force(expr); NA_character_ },
                  error = function(e) conditionMessage(e))
  check(label, !is.na(msg) && grepl(pattern, msg, fixed = TRUE),
        if (is.na(msg)) "no error raised" else msg)
}

# A file input as Shiny would supply it
upload <- function(paths) {
  data.frame(name = basename(paths), size = 1L, type = "text/csv",
             datapath = paths, stringsAsFactors = FALSE)
}

is_pdf <- function(path) {
  isTRUE(file.exists(path)) && identical(rawToChar(readBin(path, "raw", 5)), "%PDF-")
}

run_suite <- function(files) {
  for (f in files) {
    cat("\n", basename(f), "\n", sep = "")
    source(f, local = new.env(parent = globalenv()))
  }
  cat(sprintf("\n%d passed, %d failed\n", .results$pass, .results$fail))
  if (.results$fail > 0L) quit(status = 1L)
}
