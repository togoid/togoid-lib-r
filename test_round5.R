#!/usr/bin/env Rscript
# Round 5 feedback verification tests
# Run: Rscript test_round5.R

suppressPackageStartupMessages(library(togoid))

pass <- function(name) cat(sprintf("[PASS] %s\n", name))
fail <- function(name, msg = "") cat(sprintf("[FAIL] %s %s\n", name, msg))

cat("\n========== R5-A: Large convert input (POST) ==========\n")
# Simulate the feedback scenario: ncbigene -> uniprot with thousands of IDs
big_ids <- as.character(seq_len(6500))
res_a <- tryCatch(
  togoid_convert(ids = big_ids, route = c("ncbigene", "uniprot")),
  error = function(e) conditionMessage(e)
)
if (is.data.frame(res_a)) {
  pass(sprintf("convert with %d IDs completed (rows=%d)", length(big_ids), nrow(res_a)))
} else {
  fail(sprintf("convert with %d IDs failed", length(big_ids)),
       paste(res_a, collapse = " "))
}

cat("\n========== R5-B: Large count input (POST) ==========\n")
# count is exposed via the R6 method (no togoid_count wrapper).
# 1000 IDs is enough to exceed typical URL-length limits (used to break GET) but
# stays under server-side processing limits for /count.
count_ids <- as.character(seq_len(1000))
converter <- TogoIDConverter$new()
res_b <- tryCatch(
  converter$count(src = "ncbigene", dst = "uniprot", ids = count_ids),
  error = function(e) conditionMessage(e)
)
if (is.list(res_b) || is.numeric(res_b)) {
  pass(sprintf("count with %d IDs completed", length(count_ids)))
  print(res_b)
} else {
  fail("count failed", paste(res_b, collapse = " "))
}

cat("\n========== R5-C: Large label2id input (POST) ==========\n")
# 500 labels exceeds typical GET URL limits when joined with '|'
many_labels <- rep("Lung Cancer", 500)
res_c <- tryCatch(
  togoid_label2id(labels = many_labels, dataset = "mondo"),
  error = function(e) conditionMessage(e)
)
if (is.data.frame(res_c) && nrow(res_c) > 0) {
  pass(sprintf("label2id with %d labels completed (rows=%d)",
               length(many_labels), nrow(res_c)))
} else {
  fail("label2id failed", paste(res_c, collapse = " "))
}

cat("\n========== R5-D: annotate filter excludes non-matching IDs ==========\n")
# Feedback regression: filter must remove IDs that don't satisfy the filter
res_d <- tryCatch(
  togoid_annotate(
    dataset = "go", ids = c("0005643", "0097110"),
    fields = c("label", "go_aspect"),
    filters = list(go_aspect = c("molecular_function"))
  ),
  error = function(e) conditionMessage(e)
)
if (is.data.frame(res_d)) {
  print(res_d)
  if (nrow(res_d) == 1 && res_d$id[1] == "0097110") {
    pass("filter excludes non-matching ID (1 row, only 0097110)")
  } else {
    fail(sprintf("filter mis-behavior: nrow=%d ids=%s",
                 nrow(res_d), paste(res_d$id, collapse = ",")))
  }
} else {
  fail("annotate with filter failed", paste(res_d, collapse = " "))
}

cat("\n========== R5-E: annotate without filter still pads missing IDs ==========\n")
res_e <- tryCatch(
  togoid_annotate(dataset = "ncbigene", ids = c("1", "8", "9"),
                  fields = c("label", "gene_synonym")),
  error = function(e) conditionMessage(e)
)
if (is.data.frame(res_e)) {
  print(res_e)
  if (nrow(res_e) == 3 && "8" %in% res_e$id) {
    row8 <- res_e[res_e$id == "8", ]
    if (is.na(row8$label)) {
      pass("ID 8 padded as NA row when no filter is specified")
    } else {
      fail("ID 8 row exists but label is not NA")
    }
  } else {
    fail(sprintf("expected 3 rows including ID 8, got nrow=%d", nrow(res_e)))
  }
} else {
  fail("annotate failed", paste(res_e, collapse = " "))
}

cat("\n========== R5-F: Round 4 regression (Web UI columns still work) ==========\n")
# Re-verify the round-4 PubDictionaries layout survives POST migration
res_f <- tryCatch(
  togoid_label2id(labels = c("Lung Cancer"), dataset = "mondo"),
  error = function(e) conditionMessage(e)
)
if (is.data.frame(res_f) && "Name" %in% colnames(res_f) &&
    !"dictionary" %in% colnames(res_f)) {
  pass("Web UI column layout preserved (Name present, dictionary removed)")
  print(res_f)
} else {
  fail("Round 4 Web UI layout broke", paste(class(res_f), collapse = ","))
}

cat("\nAll tests completed.\n")
