#!/usr/bin/env Rscript
# Debug test 2

devtools::load_all(quiet = TRUE)

cat("Debugging test 2...\n")

# Make the API request directly to see the response
converter <- TogoIDConverter$new()

tryCatch({
  result <- converter$convert(
    ids = c("MONDO_0005570", "MONDO_0004471", "MONDO_0000115"),
    route = c("mondo", "doid"),
    format = "json",
    report = "full"
  )

  cat("API Response:\n")
  print(str(result))

  cat("\nResults:\n")
  print(result$results)

  cat("\nLengths of each result:\n")
  for (i in seq_along(result$results)) {
    cat("  Result", i, "length:", length(result$results[[i]]), "\n")
    cat("    Content:", paste(result$results[[i]], collapse = ", "), "\n")
  }

}, error = function(e) {
  cat("ERROR:", conditionMessage(e), "\n")
  cat("Traceback:\n")
  print(traceback())
})
