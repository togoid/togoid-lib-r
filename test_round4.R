#!/usr/bin/env Rscript
# Round 4 feedback verification tests
# Run: Rscript test_round4.R

suppressPackageStartupMessages(library(togoid))

pass <- function(name) cat(sprintf("[PASS] %s\n", name))
fail <- function(name, msg = "") cat(sprintf("[FAIL] %s %s\n", name, msg))

cat("\n========== R-A: format=table/json must error ==========\n")
err1 <- tryCatch(
  togoid_convert(ids = c("1", "9"), route = c("ncbigene", "ensembl_gene"), format = "table"),
  error = function(e) conditionMessage(e)
)
if (is.character(err1) && grepl("Unsupported format", err1)) {
  pass("format='table' raises error")
  cat("       ->", err1, "\n")
} else {
  fail("format='table' should have raised error", paste(err1, collapse = " "))
}

err2 <- tryCatch(
  togoid_convert(ids = c("1", "9"), route = c("ncbigene", "ensembl_gene"), format = "json"),
  error = function(e) conditionMessage(e)
)
if (is.character(err2) && grepl("Unsupported format", err2)) {
  pass("format='json' raises error")
} else {
  fail("format='json' should have raised error", paste(err2, collapse = " "))
}

cat("\n========== R-B: label_types as vector ==========\n")
res_b <- tryCatch(
  togoid_label2id(labels = c("BRCA1"), dataset = "ncbigene", taxonomy = "9606",
                  label_types = c("symbol", "synonym")),
  error = function(e) conditionMessage(e)
)
if (is.data.frame(res_b) && nrow(res_b) > 0) {
  pass("label_types=c('symbol','synonym') accepted")
  print(head(res_b, 3))
} else {
  fail("label_types vector failed", paste(res_b, collapse = " "))
}

cat("\n========== R-C: PubDictionaries Web UI format ==========\n")
res_c <- tryCatch(
  togoid_label2id(labels = c("Lung Cancer"), dataset = "mondo"),
  error = function(e) conditionMessage(e)
)
if (is.data.frame(res_c)) {
  cat("Columns:", paste(colnames(res_c), collapse = ", "), "\n")
  print(res_c)
  has_name_col <- "Name" %in% colnames(res_c)
  has_no_dictionary <- !"dictionary" %in% colnames(res_c)
  has_match_label <- any(c("Name", "Exact synonym", "Broad synonym") %in% res_c$match_type)
  if (has_name_col) pass("preferred label column 'Name' exists")
  else fail("preferred label column 'Name' missing")
  if (has_no_dictionary) pass("'dictionary' column removed")
  else fail("'dictionary' column should be removed")
  if (has_match_label) pass("match_type uses human-readable labels (Name/Exact synonym/Broad synonym)")
  else fail("match_type should use dataset.yaml label values", paste(unique(res_c$match_type), collapse = ","))

  broad_row <- res_c[res_c$match_type == "Broad synonym", ]
  if (nrow(broad_row) > 0 && !is.na(broad_row$Name[1]) && nzchar(broad_row$Name[1])) {
    pass(sprintf("canonical label resolved for Broad synonym: '%s'", broad_row$Name[1]))
  } else {
    fail("canonical label not resolved for Broad synonym")
  }
} else {
  fail("PubDictionaries query failed", paste(res_c, collapse = " "))
}

cat("\n========== R-D: annotate vector cells (togoid_annotate) ==========\n")
res_d <- tryCatch(
  togoid_annotate(dataset = "ncbigene", ids = c("1", "9"),
                  fields = c("label", "gene_synonym")),
  error = function(e) conditionMessage(e)
)
if (is.data.frame(res_d)) {
  print(res_d)
  if (is.list(res_d$gene_synonym) && length(res_d$gene_synonym[[1]]) > 1) {
    pass(sprintf("gene_synonym is list-column, first cell length=%d",
                 length(res_d$gene_synonym[[1]])))
    cat("       gene_synonym[[1]]:", paste(res_d$gene_synonym[[1]], collapse = " | "), "\n")
  } else {
    fail("gene_synonym should be list-column with vector cells")
  }
} else {
  fail("togoid_annotate failed", paste(res_d, collapse = " "))
}

cat("\n========== R-E: annotate missing field -> NA ==========\n")
res_e <- tryCatch(
  togoid_annotate(dataset = "taxonomy", ids = c("7227"),
                  fields = c("label", "common_name")),
  error = function(e) conditionMessage(e)
)
if (is.data.frame(res_e)) {
  print(res_e)
  if ("common_name" %in% colnames(res_e)) {
    pass("missing common_name field returns NA without error")
  } else {
    fail("common_name column missing entirely")
  }
} else {
  fail("annotate with missing field errored", paste(res_e, collapse = " "))
}

cat("\n========== R-F: annotate missing IDs -> NA rows ==========\n")
res_f <- tryCatch(
  togoid_annotate(dataset = "ncbigene", ids = c("1", "8", "9"),
                  fields = c("label", "gene_synonym")),
  error = function(e) conditionMessage(e)
)
if (is.data.frame(res_f)) {
  print(res_f)
  if ("8" %in% res_f$id) {
    row8 <- res_f[res_f$id == "8", ]
    if (is.na(row8$label)) {
      pass("missing ID '8' present with NA values")
    } else {
      fail("missing ID '8' should have NA label")
    }
  } else {
    fail("missing ID '8' not in output")
  }
} else {
  fail("annotate failed", paste(res_f, collapse = " "))
}

cat("\n========== R-D2: togoid_convert(annotate) vector cells ==========\n")
res_d2 <- tryCatch(
  togoid_convert(
    ids = c("1", "9"),
    route = c("ncbigene", "ensembl_gene"),
    annotate = list(c("ncbigene", "gene_synonym"))
  ),
  error = function(e) conditionMessage(e)
)
if (is.data.frame(res_d2)) {
  print(res_d2)
  cat("Columns:", paste(colnames(res_d2), collapse = ", "), "\n")
  if ("ncbigene.gene_synonym" %in% colnames(res_d2)) {
    syn_col <- res_d2[["ncbigene.gene_synonym"]]
    if (is.list(syn_col) && length(syn_col[[1]]) > 1) {
      pass(sprintf("annotation column is list-column, first cell length=%d",
                   length(syn_col[[1]])))
    } else {
      fail("annotation column should be a list-column with vectors")
    }
  } else {
    fail("expected column ncbigene.gene_synonym not found")
  }
} else {
  fail("togoid_convert with annotate failed", paste(res_d2, collapse = " "))
}

cat("\nAll tests completed.\n")
