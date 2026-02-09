#!/usr/bin/env Rscript

# TogoID R Package - Package Skeleton Creation Script

library(usethis)

cat("Creating TogoID R package skeleton...\n\n")

# Set working directory
setwd("/home/souta/projects")

# Create package structure
cat("1. Creating package structure...\n")
create_package("togoid-lib-r", open = FALSE)

# Change to package directory
setwd("/home/souta/projects/togoid-lib-r")

# Set up Git
cat("2. Initializing Git...\n")
use_git()

# Set up license
cat("3. Setting up MIT license...\n")
use_mit_license("DBCLS")

# Set up README
cat("4. Creating README...\n")
use_readme_rmd()

# Set up testing
cat("5. Setting up testthat...\n")
use_testthat()

# Set up package dependencies
cat("6. Adding package dependencies...\n")
use_package("R6", type = "Imports")
use_package("httr2", type = "Imports")
use_package("jsonlite", type = "Imports")
use_package("dplyr", type = "Imports")
use_package("tibble", type = "Imports")
use_package("cli", type = "Imports")
use_package("rlang", type = "Imports")

# Suggested packages
use_package("ghql", type = "Suggests")
use_package("testthat", type = "Suggests")
use_package("covr", type = "Suggests")

# Create directory structure
cat("7. Creating directory structure...\n")
dir.create("R", showWarnings = FALSE)
dir.create("man", showWarnings = FALSE)
dir.create("tests/testthat", recursive = TRUE, showWarnings = FALSE)
dir.create("vignettes", showWarnings = FALSE)
dir.create("inst/exec", recursive = TRUE, showWarnings = FALSE)

# Create initial R files
cat("8. Creating initial R files...\n")

# Package documentation
use_package_doc()

# Create placeholder files
file.create("R/utils.R")
file.create("R/converter.R")
file.create("R/annotations.R")
file.create("R/label_converter.R")

cat("\n=== Package Creation Complete! ===\n")
cat("Package location: /home/souta/projects/togoid-lib-r\n")
cat("\nNext steps:\n")
cat("1. Implement core functionality in R/ directory\n")
cat("2. Add documentation with roxygen2 comments\n")
cat("3. Write tests in tests/testthat/\n")
cat("4. Run devtools::check() to verify package\n")
