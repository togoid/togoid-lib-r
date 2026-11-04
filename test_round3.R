#!/usr/bin/env Rscript
# Round 3 feedback tests

library(togoid)

cat("\n========================================\n")
cat("Test R7: config_list_targets reverse links\n")
cat("========================================\n")
tryCatch({
  targets <- togoid_config_list_targets("tair")
  cat("Targets for tair:", paste(targets, collapse=", "), "\n")
  if (length(targets) > 0) {
    cat("SUCCESS: Found", length(targets), "targets (including reverse links)\n")
  } else {
    cat("FAILED: No targets found\n")
  }
}, error = function(e) cat("ERROR:", conditionMessage(e), "\n"))

cat("\n========================================\n")
cat("Test R4: label2id with unmatched labels\n")
cat("========================================\n")
tryCatch({
  result <- togoid_label2id(labels = c("AR", "dummy"), dataset = "ncbigene", taxonomy = "9606")
  print(result)
  if ("dummy" %in% result$input) {
    cat("SUCCESS: 'dummy' included as Unmatched\n")
  } else {
    cat("FAILED: 'dummy' not found in results\n")
  }
}, error = function(e) cat("ERROR:", conditionMessage(e), "\n"))

cat("\n========================================\n")
cat("Test R2: annotation column names\n")
cat("========================================\n")
tryCatch({
  result <- togoid_convert(
    ids = c("1", "9"),
    route = c("ncbigene", "ensembl_gene"),
    annotate = list(c("ncbigene", "label"))
  )
  print(result)
  cat("Column names:", paste(colnames(result), collapse=", "), "\n")
  if ("ncbigene.label" %in% colnames(result)) {
    cat("SUCCESS: Annotation column named correctly\n")
  } else {
    cat("FAILED: Expected 'ncbigene.label' column\n")
  }
}, error = function(e) cat("ERROR:", conditionMessage(e), "\n"))

cat("\n========================================\n")
cat("Test R3: annotate with NA values\n")
cat("========================================\n")
tryCatch({
  result <- togoid_convert(
    ids = c("1", "8", "9"),
    route = c("ncbigene", "ensembl_gene"),
    annotate = list(c("ensembl_gene", "label"))
  )
  print(result)
  cat("SUCCESS: No error with NA values\n")
}, error = function(e) cat("ERROR:", conditionMessage(e), "\n"))

cat("\n========================================\n")
cat("Test R1: format=table -> dataframe\n")
cat("========================================\n")
tryCatch({
  result <- togoid_convert(
    ids = c("1", "9"),
    route = c("ncbigene", "ensembl_gene"),
    format = "table"
  )
  cat("Class:", class(result), "\n")
  if (is.data.frame(result)) {
    cat("SUCCESS: format='table' now returns dataframe\n")
  } else {
    cat("FAILED: Expected dataframe\n")
  }
}, error = function(e) cat("ERROR:", conditionMessage(e), "\n"))

cat("\n========================================\n")
cat("Test R8: get_ortholog with missing conversions\n")
cat("========================================\n")
tryCatch({
  result <- togoid_get_ortholog(
    ids = c("P01023", "Q400J6"),
    route = c("uniprot", "oma_group"),
    target_taxids = c("10090", "10116")
  )
  print(result)
  if ("Q400J6" %in% result$source_id) {
    cat("SUCCESS: Missing ID included with NA\n")
  } else {
    cat("NOTE: Q400J6 not in result\n")
  }
}, error = function(e) cat("ERROR:", conditionMessage(e), "\n"))

cat("\n========================================\n")
cat("Test R5/R6: PubDictionaries with mondo\n")
cat("========================================\n")
tryCatch({
  result <- togoid_label2id(
    labels = c("lung cancer", "anemia", "gout"),
    dataset = "mondo"
  )
  print(result)
  cat("SUCCESS: PubDictionaries request completed\n")
}, error = function(e) cat("ERROR:", conditionMessage(e), "\n"))

cat("\nAll tests completed.\n")
