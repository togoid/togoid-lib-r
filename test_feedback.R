#!/usr/bin/env Rscript
# Test script for user feedback fixes

# Load package
devtools::load_all(quiet = TRUE)

cat("=== Testing User Feedback Fixes ===\n\n")

# Test 1: togoid_convert with report="full" (default)
cat("1. Testing togoid_convert with default report='full'\n")
cat("   Converting MONDO IDs to DOID...\n")
result1 <- togoid_convert(
  ids = c("MONDO_0005570", "MONDO_0004471"),
  route = c("mondo", "doid")
)
print(result1)
cat("   Column names should be: mondo, doid\n")
cat("   Actual column names:", colnames(result1), "\n\n")

# Test 2: togoid_convert with IDs that have no conversion target
cat("2. Testing togoid_convert with IDs that have no conversion (should not error)\n")
result2 <- tryCatch({
  togoid_convert(
    ids = c("MONDO_0005570", "MONDO_0004471", "MONDO_0000115"),
    route = c("mondo", "doid")
  )
}, error = function(e) {
  cat("   ERROR:", conditionMessage(e), "\n")
  NULL
})
if (!is.null(result2)) {
  cat("   Success! No error occurred.\n")
  print(result2)
} else {
  cat("   FAILED: Error occurred\n")
}
cat("\n")

# Test 3: togoid_label2id default output format
cat("3. Testing togoid_label2id default format (should be dataframe)\n")
result3 <- togoid_label2id(
  labels = c("BRCA1", "TP53"),
  dataset = "ncbigene",
  taxonomy = "9606"
)
cat("   Result class:", class(result3), "\n")
cat("   Is data.frame?", is.data.frame(result3), "\n")
if (is.data.frame(result3)) {
  print(head(result3))
} else {
  cat("   FAILED: Not a data.frame\n")
}
cat("\n")

# Test 4: togoid_annotate default output format
cat("4. Testing togoid_annotate default format (should be dataframe)\n")
result4 <- togoid_annotate(
  dataset = "ncbigene",
  ids = c("672", "7157"),
  fields = c("label")
)
cat("   Result class:", class(result4), "\n")
cat("   Is data.frame?", is.data.frame(result4), "\n")
if (is.data.frame(result4)) {
  print(head(result4))
} else {
  cat("   FAILED: Not a data.frame\n")
}
cat("\n")

# Test 5: togoid_annotate with filter
cat("5. Testing togoid_annotate with filter parameter\n")
cat("   Filter syntax: filters = list(field_name = c('value1', 'value2'))\n")
result5 <- tryCatch({
  togoid_annotate(
    dataset = "ncbigene",
    ids = c("672", "7157"),
    fields = c("label", "gene_type_name"),
    filters = list(gene_type_name = c("protein-coding"))
  )
}, error = function(e) {
  cat("   ERROR:", conditionMessage(e), "\n")
  NULL
})
if (!is.null(result5)) {
  cat("   Filter applied successfully\n")
  print(head(result5))
} else {
  cat("   Note: Filter test may fail if field doesn't exist\n")
}
cat("\n")

# Test 6: togoid_config_list_targets
cat("6. Testing togoid_config_list_targets (new function)\n")
result6 <- tryCatch({
  togoid_config_list_targets("ncbigene")
}, error = function(e) {
  cat("   ERROR:", conditionMessage(e), "\n")
  NULL
})
if (!is.null(result6)) {
  cat("   Success! Found", length(result6), "target datasets\n")
  cat("   First 5 targets:", head(result6, 5), "\n")
} else {
  cat("   FAILED: Function not implemented or error occurred\n")
}
cat("\n")

cat("=== All tests completed ===\n")
