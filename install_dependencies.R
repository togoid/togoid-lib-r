#!/usr/bin/env Rscript

# TogoID R Package - Dependency Installation Script
# This script installs all required packages for development and runtime

cat("Installing TogoID R package dependencies...\n\n")

# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Ensure user library exists
user_lib <- Sys.getenv("R_LIBS_USER")
if (!dir.exists(user_lib)) {
  dir.create(user_lib, recursive = TRUE)
  cat(sprintf("Created user library: %s\n", user_lib))
}
.libPaths(c(user_lib, .libPaths()))

# Development tools
dev_packages <- c(
  "devtools",      # Package development
  "usethis",       # Project setup
  "roxygen2",      # Documentation generation
  "testthat",      # Testing framework
  "pkgdown",       # Website generation
  "covr"           # Code coverage
)

# Runtime dependencies
runtime_packages <- c(
  "R6",            # OOP system
  "httr2",         # HTTP client (modern)
  "jsonlite",      # JSON processing
  "dplyr",         # Data manipulation
  "tibble",        # Modern data frames
  "cli",           # CLI UI
  "rlang"          # Programming utilities
)

# Optional packages
optional_packages <- c(
  "ghql",          # GraphQL client
  "docopt"         # CLI argument parsing
)

# Function to install packages if not already installed
install_if_missing <- function(packages, type = "required") {
  cat(sprintf("\nChecking %s packages...\n", type))

  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      cat(sprintf("  Installing %s...\n", pkg))
      tryCatch(
        {
          install.packages(pkg, quiet = FALSE, lib = .libPaths()[1])
          cat(sprintf("  ✓ %s installed successfully\n", pkg))
        },
        error = function(e) {
          cat(sprintf("  ✗ Failed to install %s: %s\n", pkg, conditionMessage(e)))
        }
      )
    } else {
      cat(sprintf("  ✓ %s already installed\n", pkg))
    }
  }
}

# Install packages
install_if_missing(dev_packages, "development")
install_if_missing(runtime_packages, "runtime")
install_if_missing(optional_packages, "optional")

cat("\n=== Installation Summary ===\n")
cat("Development tools: ", length(dev_packages), " packages\n")
cat("Runtime dependencies: ", length(runtime_packages), " packages\n")
cat("Optional packages: ", length(optional_packages), " packages\n")

cat("\nDependency installation complete!\n")
cat("You can now create the package with:\n")
cat("  Rscript create_package.R\n")
