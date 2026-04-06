#!/usr/bin/env Rscript
# Test script for additional feedback fixes

# Load package
devtools::load_all(quiet = TRUE)

cat("=== Testing Additional Feedback Fixes ===\n\n")

# Test 1: togoid_label2id returns multiple IDs for same input
cat("1. Testing togoid_label2id with multiple IDs for same input\n")
result1 <- togoid_label2id(
  labels = c("AR", "OCT4"),
  dataset = "ncbigene",
  taxonomy = "9606"
)
cat("   Number of results:\n")
cat("   AR:", sum(result1$input == "AR"), "(should be > 1)\n")
cat("   OCT4:", sum(result1$input == "OCT4"), "(should be > 1)\n")
print(result1)
cat("\n")

# Test 2: togoid_label2id requires taxonomy for ncbigene
cat("2. Testing togoid_label2id taxonomy requirement\n")
result2 <- tryCatch({
  togoid_label2id(
    labels = c("AR", "OCT4"),
    dataset = "ncbigene"
    # taxonomy not specified
  )
  cat("   FAILED: Should have raised an error\n")
  NULL
}, error = function(e) {
  cat("   SUCCESS: Error raised as expected\n")
  cat("   Error message:", conditionMessage(e), "\n")
  TRUE
})
cat("\n")

# Test 3: PubDictionaries with use_ngram_similarity
cat("3. Testing PubDictionaries with use_ngram_similarity\n")
result3 <- tryCatch({
  togoid_label2id(
    labels = c("ATP", "water"),
    dataset = "chebi"
  )
  cat("   SUCCESS: No error occurred\n")
  cat("   Number of results:", nrow(result3), "\n")
  print(head(result3))
  TRUE
}, error = function(e) {
  cat("   FAILED: Error occurred\n")
  cat("   Error message:", conditionMessage(e), "\n")
  NULL
})
cat("\n")

cat("=== All additional feedback tests completed ===\n")
