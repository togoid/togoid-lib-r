# TogoID R Library

R library for biological database ID conversion and annotation using [TogoID](https://togoid.dbcls.jp/).

## Features

- **ID Conversion**: Convert IDs between biological databases
- **Ortholog Retrieval**: Get orthologs through round-trip conversion
- **Label to ID**: Convert biological labels (gene names, etc.) to database IDs
- **Annotations**: Get labels and annotations for database IDs
- **Multiple Formats**: Support for dataframe, tibble, table, list, and JSON
- **Dual Interface**: Use as R library with both R6 classes and functional wrappers
- **Comprehensive**: Search databases, find routes, get configurations
- **Enrichment Analysis**: Turn any conversion route into gene sets, test them for over-representation, and draw the result on a single-cell UMAP

## Installation

### From GitHub

```r
# Install devtools if needed
install.packages("devtools")

# Install togoid from GitHub
devtools::install_github("togoid/togoid-lib-r")
```

### From Source

```bash
# Clone the repository
git clone https://github.com/togoid/togoid-lib-r.git
cd togoid-lib-r

# Install dependencies
Rscript install_dependencies.R

# Install the package
R CMD INSTALL .

# Or using devtools in R
devtools::install()
```

## Quick Start

### Basic ID Conversion

```r
library(togoid)

# Functional style - convert ncbigene to ensembl_gene
result <- togoid_convert(
  ids = c("1", "9"),
  route = c("ncbigene", "ensembl_gene")
)
print(result)
#   ncbigene  ensembl_gene
# 1        1 ENSG00000121410
# 2        9 ENSG00000171428

# Note: Column names use dataset names from the route
# Default format is "dataframe" with report="full"

# Using pipe operator
c("1", "9") |>
  togoid_convert(route = c("ncbigene", "ensembl_gene"))

# R6 class style
converter <- TogoIDConverter$new()
result <- converter$convert(
  ids = c("1", "9"),
  route = c("ncbigene", "ensembl_gene"),
  format = "tibble"
)
```

### Get Orthologs

```r
# Get mouse and rat orthologs for human genes
orthologs <- togoid_get_ortholog(
  ids = c("672", "7157"),
  route = c("ncbigene", "homologene"),
  target_taxids = c("10090", "10116"),  # Mouse and Rat
  format = "dataframe"
)
print(orthologs)
#   source_id intermediate_id target_id taxonomy_id
# 1       672           11167     11167       10090
# 2       672           11167    140656       10116
# 3      7157           37329    116632       10116
# 4      7157           37329     17961       10090
```

### Label to ID Conversion

```r
# Convert gene symbols to ncbigene IDs
# Default format is now "dataframe"
result <- togoid_label2id(
  labels = c("BRCA1", "TP53", "EGFR"),
  dataset = "ncbigene",
  taxonomy = "9606"  # Human
)
print(result)
#   input match_type symbol identifier taxonomy
# 1 BRCA1     symbol  BRCA1        672     9606
# 2  TP53     symbol   TP53       7157     9606
# 3  EGFR     symbol   EGFR       1956     9606

# Extract IDs
ids <- result$identifier
print(ids)
# [1] "672"  "7157" "1956"
```

### Get Annotations

```r
# List available fields
fields <- togoid_list_fields("ncbigene")
print(fields)

# Get annotations (default format is now "dataframe")
annotations <- togoid_annotate(
  dataset = "ncbigene",
  ids = c("672", "7157"),
  fields = c("label", "gene_synonym")
)
print(annotations)
#    id label              gene_synonym
# 1 672 BRCA1 RNF53, BRCC1, FANCS, ...
# 2 7157 TP53  P53, LFS1, TRP53, ...

# With filters (R uses named lists)
annotations <- togoid_annotate(
  dataset = "go",
  ids = c("GO:0005643", "GO:0097110"),
  fields = c("label", "go_aspect"),
  filters = list(go_aspect = c("molecular_function"))
)
```

### Search and Route

```r
# Search databases
databases <- togoid_search_databases("uniprot")

# Find routes between databases
routes <- togoid_route(
  src = "ncbigene",
  dst = "uniprot",
  max_hops = 3
)

# List target datasets reachable from a source in one hop
targets <- togoid_config_list_targets("ncbigene")
print(targets)
# [1] "ensembl_gene" "ensembl_protein" "ensembl_transcript" ...
```

## Usage Examples

### Different Output Formats

```r
# List format
result <- togoid_convert(
  ids = c("1", "9"),
  route = c("ncbigene", "ensembl_gene"),
  format = "list"
)

# Table format (list of character vectors)
result <- togoid_convert(
  ids = c("1", "9"),
  route = c("ncbigene", "ensembl_gene"),
  format = "table"
)

# Tibble format
result <- togoid_convert(
  ids = c("1", "9"),
  route = c("ncbigene", "ensembl_gene"),
  format = "tibble"
)

# JSON format
result <- togoid_convert(
  ids = c("1", "9"),
  route = c("ncbigene", "ensembl_gene"),
  format = "json"
)
```

### Using R6 Classes

```r
# TogoIDConverter
converter <- TogoIDConverter$new()

# Convert IDs
result <- converter$convert(
  ids = c("1", "9"),
  route = c("ncbigene", "ensembl_gene")
)

# Count mappings
count_info <- converter$count(
  src = "ncbigene",
  dst = "ensembl_gene",
  ids = c("1", "9")
)

# Get configuration
config <- converter$config_dataset("ncbigene")

# AnnotationsConverter
annotator <- AnnotationsConverter$new()

fields <- annotator$list_fields("ncbigene")
annotations <- annotator$execute_query(
  dataset_name = "ncbigene",
  ids = c("672", "7157"),
  fields = c("label", "gene_synonym")
)

# LabelConverter
label_conv <- LabelConverter$new(verbose = TRUE)

results <- label_conv$convert(
  labels = c("BRCA1", "TP53"),
  dataset = "ncbigene",
  taxonomy = "9606"
)
```

### Functional Programming Style

```r
library(dplyr)

# Chain operations with pipes
gene_symbols <- c("BRCA1", "TP53", "EGFR")

# togoid_label2id() returns a data.frame, so pull the identifier column directly
result <- gene_symbols |>
  togoid_label2id(dataset = "ncbigene", taxonomy = "9606") |>
  (\(df) df$identifier[!is.na(df$identifier)])() |>
  togoid_convert(route = c("ncbigene", "ensembl_gene"))

# Using with dplyr
library(tibble)

data <- tibble(
  gene = c("BRCA1", "TP53", "EGFR")
)

data <- data |>
  mutate(
    ncbigene_id = sapply(gene, function(g) {
      res <- togoid_label2id(g, "ncbigene", "9606")
      res$identifier[1]
    })
  )
```

## API Reference

### Main Classes

#### TogoIDConverter

- `convert(ids, route, format, report, ...)` - Convert IDs between databases (default: format="dataframe", report="full")
- `get_ortholog(ids, route, target_taxids, format)` - Get orthologs
- `search_databases(name)` - Search databases by name
- `search_id(id_string)` - Search databases by ID pattern
- `lookup_id(id_string)` - Lookup which tables contain an ID
- `route(src, dst, max_hops)` - Find conversion routes
- `count(src, dst, ids, link)` - Count mappings
- `config_dataset(name)` - Get dataset configuration
- `config_relation(src, dst)` - Get relation configuration
- `config_descriptions()` - Get database descriptions
- `config_statistics()` - Get database statistics
- `config_taxonomy()` - Get taxonomy list
- `config_list_targets(source)` - List target datasets reachable in one hop

#### AnnotationsConverter

- `get_dataset(dataset_name)` - Get dataset configuration
- `list_fields(dataset_name)` - List available annotation fields
- `execute_query(dataset_name, ids, fields, filters, format)` - Execute GraphQL query (default format: "dataframe")
- `build_rows(dataset_label, fields, field_meta, records, filters, compact)` - Build table rows

#### LabelConverter

- `convert(labels, dataset, taxonomy, format, ...)` - Convert labels to IDs (auto-detects API, default format: "dataframe")
- `convert_pubdictionaries(labels, dictionaries, tags, threshold, ...)` - Use PubDictionaries API
- `convert_sparqlist(labels, sparqlist, label_types, taxonomy)` - Use SPARQList API

### Wrapper Functions

- `togoid_convert(ids, route, format, ...)` - ID conversion (default: format="dataframe", report="full")
- `togoid_get_ortholog(ids, route, target_taxids, format)` - Ortholog retrieval
- `togoid_annotate(dataset, ids, fields, filters, format)` - Get annotations (default format: "dataframe")
- `togoid_list_fields(dataset)` - List annotation fields
- `togoid_label2id(labels, dataset, taxonomy, format, ...)` - Label to ID conversion (default format: "dataframe")
- `togoid_search_databases(name)` - Search databases
- `togoid_route(src, dst, max_hops)` - Find routes
- `togoid_config_list_targets(source)` - List target datasets reachable in one hop

## Enrichment Analysis and UMAP Visualization

`togoid` turns ID conversion into gene-set analysis. A TogoID route that ends in
an annotation dataset *is* a gene-set library: every term reached by the route
becomes a set containing the input genes that map to it. From there it is a
standard over-representation analysis, and the results can be drawn directly
onto a single-cell embedding.

The key property is that **only the route changes** between annotation databases:

```r
togoid_gene_sets(genes, route = c("ncbigene", "uniprot", "reactome_pathway"))  # pathways
togoid_gene_sets(genes, route = c("ncbigene", "uniprot", "go"))                # GO terms
togoid_gene_sets(genes, route = c("ncbigene", "medgen", "mondo"))              # diseases
```

Anything TogoID can reach works the same way. The analysis itself adds no
dependencies: the hypergeometric test is `stats::phyper()` and the FDR
correction is `stats::p.adjust()`. `ggplot2` and `patchwork` are needed only for
the figures, `Seurat` only for the adapters.

See `vignette("enrichment", package = "togoid")` for the full manual, and
`system.file("examples/scRNAseq_enrichment", package = "togoid")` for a runnable
pipeline.

### Quick Start

```r
library(togoid)

clusters <- list(
  `T cells` = c("CD3D", "CD3E", "CD3G", "IL7R", "LCK", "ZAP70", "CD2", "CD28", "LAT"),
  `B cells` = c("MS4A1", "CD79A", "CD79B", "CD19", "BLNK", "BANK1", "PAX5"),
  Myeloid   = c("LYZ", "CD14", "FCGR3A", "CSF1R", "ITGAM", "TLR2", "TLR4", "S100A8")
)
all_genes <- sort(unique(unlist(clusters, use.names = FALSE)))

gene_sets <- togoid_gene_sets(
  all_genes,
  route = c("ncbigene", "uniprot", "reactome_pathway")
)
results <- togoid_enrich_clusters(clusters, gene_sets, min_set_size = 3)

cat(togoid_enrichment_summary(results))
```

```
Cluster B cells:
  terms tested: 6
  significant (FDR < 0.05): 2
    - Antigen activates B Cell Receptor (BCR) leading to generation of second messengers [R-HSA-983695]
      FDR=1.40e-02  genes=4/4  fold=3.71

Cluster T cells:
  terms tested: 11
  significant (FDR < 0.05): 5
    - Generation of second messenger molecules [R-HSA-202433]
      FDR=4.01e-03  genes=6/6  fold=2.89
    - Translocation of ZAP-70 to Immunological synapse [R-HSA-202430]
      FDR=1.05e-02  genes=5/5  fold=2.89
```

### Three Annotation Databases

```r
# Reactome pathways: ncbigene -> uniprot -> reactome_pathway
pathways <- togoid_reactome_gene_sets(all_genes, taxonomy = "9606")

# GO terms: ncbigene -> uniprot -> go, filtered by aspect
processes <- togoid_go_gene_sets(all_genes, aspect = "biological_process")
functions <- togoid_go_gene_sets(all_genes, aspect = "molecular_function")

# MONDO diseases: ncbigene -> medgen -> mondo
diseases <- togoid_mondo_gene_sets(all_genes)
```

These presets are thin wrappers over `togoid_gene_sets()`. If you pass a route
that does not exist, the error names the working alternatives:

```
Error: Conversion along route ncbigene -> hp_phenotype failed for all 1 batch(es).
In addition: Warning message:
No direct connection between 'ncbigene' and 'hp_phenotype'.
Try one of these routes instead:
  - ncbigene -> medgen -> hp_phenotype
  - ncbigene -> nando -> hp_phenotype
```

### Caching Gene Sets

Building a library for a few thousand genes is many API calls. Save it once and
the analysis reproduces exactly, offline:

```r
togoid_save_gene_sets(gene_sets, "reactome.json")
gene_sets <- togoid_load_gene_sets("reactome.json")   # no network access
```

The JSON format is shared with the Python library's `GeneSetLibrary.save_json()`,
so a library built in either language can be read by the other.

### UMAP Visualization

```r
figure <- togoid_plot_umap_enrichment(
  embedding,          # data frame with umap_1, umap_2, cluster
  results,
  top_n = 3,
  fdr_cutoff = 0.05,
  width = 20, height = 8,
  show_centroids = TRUE,   # mark each cluster centroid
  centroid_shape = 16,     # a black filled circle
  centroid_size = 2
)
ggplot2::ggsave("umap_enrichment.pdf", figure, width = 20, height = 8)
```

The centroid markers are optional: `show_centroids = FALSE` hides them, and the
labels then move in closer, since the space they reserved is freed.
`centroid_shape` takes any ggplot2 point shape (see `?points`) — 16 is a filled
circle, 4 a cross — with `centroid_size` and `centroid_colour` to match.

The left panel is the usual cluster UMAP; the right repeats it with each
cluster's enriched terms written around its centroid, sized by `-log10(p)`.
Labels that cannot be placed without overlapping are dropped rather than drawn on
top of each other, and the panel is widened so nothing is clipped.

Pass the same `width` and `height` to both calls: the layout uses them to convert
font sizes into data units.

#### Reactome pathways

10x Genomics public PBMC data, 3,733 cells after QC, Seurat clustering.

![UMAP with enriched Reactome pathways](https://raw.githubusercontent.com/suimye/togoid-lib-r/docs-figures/umap_enrichment_reactome_r.png)

#### GO biological process

The same analysis through a different route.

![UMAP with enriched GO biological processes](https://raw.githubusercontent.com/suimye/togoid-lib-r/docs-figures/umap_enrichment_go_r.png)

Full-resolution figures and the analysis notes are collected in
[issue #1](https://github.com/suimye/togoid-lib-r/issues/1).

### Seurat Adapters

The enrichment code knows nothing about Seurat; these adapters do the translation
and load Seurat only when called.

```r
library(Seurat)

embedding <- togoid_umap_from_seurat(object)
markers <- togoid_markers_from_seurat(FindAllMarkers(object, only.pos = TRUE))

# Or from files written elsewhere, for example by a scanpy pipeline
embedding <- togoid_umap_from_csv("umap.csv")
markers <- togoid_markers_from_csv("markers.csv")
```

### Choosing a Background

This is the decision that most affects the results.

- **Default** (`background = NULL`): every gene in the library, i.e. every gene
  in your experiment that TogoID could annotate. This asks *which terms
  distinguish this cluster from the rest of the experiment* — usually the right
  question for cell types.
- **Explicit**: pass a larger universe for the conventional *over-represented
  relative to the genome* question. Expect many more significant hits.

`togoid_enrich_clusters()` shares one background across clusters, which is what
makes the FDR values comparable between them.

### Enrichment Functions

- `togoid_gene_sets(genes, route, ...)` - Build a gene-set library from any route
- `togoid_map_labels(labels, dataset, taxonomy)` - Resolve labels to IDs
- `togoid_reactome_gene_sets()`, `togoid_go_gene_sets()`, `togoid_mondo_gene_sets()` - Presets
- `togoid_enrichment_routes()`, `togoid_go_aspects()` - The preset routes and GO aspects
- `togoid_enrich(genes, gene_sets, ...)` - Over-representation for one gene list
- `togoid_enrich_clusters(cluster_genes, gene_sets, ...)` - ... for several clusters
- `togoid_significant()`, `togoid_top_terms()`, `togoid_enrichment_summary()` - Result helpers
- `togoid_save_gene_sets()`, `togoid_load_gene_sets()` - Cache a library
- `togoid_filter_gene_sets()`, `togoid_gene_set_genes()`, `togoid_term_labels()` - Library helpers
- `togoid_plot_umap_enrichment()`, `togoid_plot_umap_centroids()` - Figures
- `togoid_cluster_centroids()`, `togoid_select_terms()` - Plotting internals, exported for reuse
- `togoid_hypergeometric_pvalue()`, `togoid_fdr()`, `togoid_fold_enrichment()` - Statistics
- `togoid_umap_from_seurat()`, `togoid_markers_from_seurat()` - Seurat adapters
- `togoid_umap_from_csv()`, `togoid_markers_from_csv()`, `togoid_write_marker_lists()` - File adapters

## Configuration

### Environment Variables

- `TOGOID_API_ENDPOINT` - TogoID API base URL (default: https://api.togoid.dbcls.jp)
- `TOGOID_GRASP_ENDPOINT` - GRASP GraphQL endpoint (default: https://dx.dbcls.jp/grasp-dev-togoid)

### Custom API Endpoints

```r
# R6 classes
converter <- TogoIDConverter$new(api_base_url = "http://localhost:5000")

# Wrapper functions use environment variables
Sys.setenv(TOGOID_API_ENDPOINT = "http://localhost:5000")
result <- togoid_convert(ids = c("1", "9"), route = c("ncbigene", "ensembl_gene"))
```

## Requirements

- R >= 4.0.0
- R6
- httr2
- jsonlite
- dplyr
- tibble
- cli
- rlang

Optional, for the enrichment analysis:

- ggplot2 and patchwork - the UMAP figures
- Seurat - the single-cell adapters and the example pipeline

The enrichment analysis itself needs no extra packages: `stats::phyper()` and
`stats::p.adjust()` do the statistics.

## Testing

```r
# Run tests
devtools::test()

# Check package
devtools::check()
```

Tests that call the TogoID API are skipped on CRAN and when offline. To run them
locally:

```bash
NOT_CRAN=true Rscript -e 'devtools::test()'
```

The enrichment feature is covered by `tests/testthat/test-enrichment.R` (offline:
statistics, gene-set container, term selection, and a check that no two placed
labels overlap) and `tests/testthat/test-enrichment-api.R` (live API: all three
preset routes, the GO aspect filter, broken-route errors, and that T cell, B cell
and myeloid marker genes recover the expected biology).

## License

MIT License

## Links

- [TogoID Website](https://togoid.dbcls.jp/)
- [TogoID API](https://api.togoid.dbcls.jp/)
- [GitHub Repository](https://github.com/togoid/togoid-lib-r)
- [Python Version](https://github.com/togoid/togoid-lib-python)

## Credits

Developed by DBCLS (Database Center for Life Science)

Port from [togoid-lib-python](https://github.com/togoid/togoid-lib-python)
