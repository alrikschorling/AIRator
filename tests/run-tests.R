# Run the whole suite. From the repository root:  Rscript tests/run-tests.R
source("tests/helper.R")
run_suite(c("tests/test-allocation.R",
            "tests/test-analysis.R",
            "tests/test-manifest.R"))
