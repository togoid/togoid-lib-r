# togoid (development version)

The package version has stayed at 1.0.0 since the first commit, so every change
after it is listed here, with its date and, where there is one, the pull request
in this repository.

## Breaking changes

* `togoid_convert()` defaults to `report = "full"` (was `"pair"`), so every ID
  along the route is returned, and its columns are named after the datasets in
  the route instead of `source_id` / `target_id` (2026-03-03, #1).
* `togoid_label2id()` and `togoid_annotate()` return a data.frame by default
  instead of a list (2026-03-03, #1).
* `togoid_convert()` no longer supports `format = "json"` or `format = "table"`;
  they raise an error. Use `"dataframe"`, `"tibble"` or `"list"` (2026-04-06).
* Annotation columns added by `togoid_convert(annotate = ...)` are named
  `dataset.field`, e.g. `ncbigene.label` (2026-04-06).
* `togoid_label2id()` output for PubDictionaries datasets follows the
  PubDictionaries Web UI layout (2026-05-01): `match_type` holds the label type
  from the dataset configuration (e.g. `Name`, `Exact synonym`), the
  `dictionary` column is removed, and a column with the preferred label is
  added.
* Multi-valued annotation fields are list-columns instead of comma-separated
  strings, in both `togoid_annotate()` and `togoid_convert(annotate = ...)`
  (2026-05-01).

## New features

* **Experimental:** enrichment analysis that uses a TogoID route as a gene-set
  library, with UMAP visualisation: `togoid_gene_sets()`,
  `togoid_enrich_clusters()`, `togoid_plot_umap_enrichment()` and related
  functions, and a runnable scRNA-seq example in
  `inst/examples/scRNAseq_enrichment/` (2026-11-04, #4; example input changed
  to the TENxPBMCData pbmc4k data on 2026-11-24, #5). This is an illustrative
  extension, not part of TogoID's core functionality. See
  `vignette("enrichment", package = "togoid")`.
* `togoid_convert()` takes `annotate` and `filter` to add annotation columns
  and keep only rows matching annotation values (2026-03-17).
* `togoid_annotate()` takes `filters`, a named list of allowed values
  (2026-03-03, #1).
* `togoid_config_list_targets()` and `TogoIDConverter$config_list_targets()`:
  datasets reachable from a source dataset in one hop (2026-03-03, #1). Datasets
  reachable through reverse links are included (2026-04-06).
* `togoid_label2id()` takes `label_types`, as a character vector
  (2026-04-06, 2026-05-01).

## Other changes

* `convert`, `count` and `label2id` send form-encoded POST requests instead of
  GET, so large inputs no longer fail with HTTP 494 (URL too long) (2026-05-25).
* `togoid_label2id()` returns every match for an input label, not only the
  first (2026-03-17).
* IDs that cannot be converted or annotated are kept as rows of `NA` in
  `togoid_get_ortholog()` (2026-04-06) and `togoid_annotate()` (2026-05-01)
  instead of raising an error; `togoid_annotate()` returns `NA` for missing
  fields.
* Errors from the TogoID API include the message the API returns, not only the
  HTTP status (2026-09-07).
* Packaging for CRAN: the LICENSE stub, `.Rbuildignore` entries for development
  files, runnable examples for functions that need no network access, and fixes
  for the `R CMD check` errors (2026-11-04, #3 and #4).

## Bug fixes

* `TogoIDConverter$lookup_id()` called a non-existent API path and always
  failed with HTTP 404.
* `togoid_label2id()` reports an error when a dataset that requires a taxonomy
  is given none, and PubDictionaries lookups use n-gram similarity
  (2026-03-17).
* `togoid_label2id()` no longer fails on unmatched labels, and
  `togoid_convert(annotate = ...)` no longer fails on columns containing `NA`
  (2026-04-06).
* `togoid_annotate(filters = ...)` drops the IDs a filter excludes, instead of
  keeping them as `NA` rows (2026-05-25).

# togoid 1.0.0

* Initial version (2026-02-09).
