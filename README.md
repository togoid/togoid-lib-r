# TogoID R Library

R library for biological database ID conversion and annotation using [TogoID](https://togoid.dbcls.jp/).

```r
library(togoid)

togoid_convert(ids = c("1", "9"), route = c("ncbigene", "ensembl_gene"))
#   ncbigene    ensembl_gene
# 1        1 ENSG00000121410
# 2        9 ENSG00000171428
```

## Features

- **ID conversion** between biological databases along any TogoID route
- **Annotations and filtering**: add annotation columns to conversion results, or keep only rows matching annotation values
- **Ortholog retrieval** through round-trip conversion and taxonomy filtering
- **Label to ID**: resolve labels (gene symbols, compound or disease names) to database IDs
- **Annotations** (labels and other fields) for database IDs
- Output as data.frame (default), tibble, or list
- Functional wrappers (`togoid_*()`) and R6 classes

## Installation

```r
# install.packages("devtools")
devtools::install_github("togoid/togoid-lib-r")
```

From a clone of the repository:

```bash
git clone https://github.com/togoid/togoid-lib-r.git
cd togoid-lib-r
Rscript install_dependencies.R
R CMD INSTALL .
```

## Usage

### ID Conversion

```r
library(togoid)

# Output formats: "dataframe" (default), "tibble", "list"
togoid_convert(ids = c("1", "9"), route = c("ncbigene", "ensembl_gene"), format = "tibble")

# Pipes work, since ids is the first argument
c("1", "9") |>
  togoid_convert(route = c("ncbigene", "ensembl_gene"))
```

Columns are named after the datasets in the route. The default `report = "full"`
returns every ID along the route; `"pair"` and `"target"` return less.

#### Annotations and Filtering

`annotate` adds annotation columns; `filter` keeps only rows whose annotation
matches one of the given values.

```r
togoid_convert(
  ids = c("1", "9"),
  route = c("ncbigene", "ensembl_gene", "ensembl_transcript"),
  annotate = list(list("ncbigene", "label")),
  filter = list(list("ensembl_transcript", "transcript_flag", c("MANE Select")))
)
#   ncbigene ncbigene.label    ensembl_gene ensembl_transcript
# 1        1           A1BG ENSG00000121410    ENST00000263100
# 2        9           NAT1 ENSG00000171428    ENST00000307719
```

#### Orthologs

```r
# ncbigene -> homologene -> ncbigene -> taxonomy, then filter by taxonomy ID
togoid_get_ortholog(
  ids = c("672", "7157"),               # Human BRCA1, TP53
  route = c("ncbigene", "homologene"),
  target_taxids = c("10090", "10116")   # Mouse and Rat
)
#   source_id intermediate_id target_id taxonomy_id
# 1       672            5276     12189       10090
# 2       672            5276    497672       10116
# 3      7157             460     22059       10090
# 4      7157             460     24842       10116
```

#### Exploring Datasets and Routes

```r
togoid_search_databases("uniprot")                         # Search databases by name
togoid_route(src = "ncbigene", dst = "uniprot", max_hops = 3)
togoid_config_list_targets("ncbigene")                     # Datasets reachable in one hop
```

The remaining API endpoints are methods of the `TogoIDConverter` R6 class:

```r
converter <- TogoIDConverter$new()
converter$count(src = "ncbigene", dst = "ensembl_gene", ids = c("1", "9"))
converter$search_id("NM_001110")      # Databases whose ID pattern matches
converter$lookup_id("672")            # Tables that contain an ID
converter$config_dataset("ncbigene")
```

Configuration of the TogoID API is also available through `config_relation()`,
`config_descriptions()`, `config_statistics()` and `config_taxonomy()`.

### Label to ID

```r
togoid_label2id(labels = c("BRCA1", "TP53", "EGFR"), dataset = "ncbigene", taxonomy = "9606")
#   input match_type symbol identifier taxonomy
# 1 BRCA1     symbol  BRCA1        672     9606
# 2  TP53     symbol   TP53       7157     9606
# 3  EGFR     symbol   EGFR       1956     9606

# Compound and disease names
togoid_label2id(labels = "caffeine", dataset = "chebi")
togoid_label2id(labels = "breast cancer", dataset = "mondo")

# From labels to converted IDs
c("BRCA1", "TP53", "EGFR") |>
  togoid_label2id(dataset = "ncbigene", taxonomy = "9606") |>
  (\(df) df$identifier[!is.na(df$identifier)])() |>
  togoid_convert(route = c("ncbigene", "ensembl_gene"))
```

The upstream service is chosen from the dataset's `label_resolver`
configuration in the TogoID API: SPARQList when it names one (e.g. `ncbigene`),
PubDictionaries otherwise (e.g. `chebi`, `mondo`). To call one of them
directly, use the `convert_sparqlist()` or `convert_pubdictionaries()` methods
of `LabelConverter`.

### Annotations

```r
togoid_list_fields("ncbigene")

togoid_annotate(
  dataset = "ncbigene",
  ids = c("672", "7157"),
  fields = c("label", "gene_synonym")
)
#     id label gene_synonym      <- multi-valued fields are list-columns
# 1  672 BRCA1 IRIS, RNF53, ...
# 2 7157  TP53 LFS1, BMFS5, ...

# Filters are a named list of allowed values
togoid_annotate(
  dataset = "go",
  ids = c("0005643", "0003677"),
  fields = c("label", "go_aspect"),
  filters = list(go_aspect = c("molecular_function"))
)
#        id       label          go_aspect
# 1 0003677 DNA binding molecular_function
```

### R6 Classes

The wrappers create an R6 object per call. To reuse one, or to set a custom
endpoint, use the classes directly; their methods take the same arguments:

| Wrapper | R6 method |
|---|---|
| `togoid_convert()`, `togoid_get_ortholog()`, `togoid_search_databases()`, `togoid_route()`, `togoid_config_list_targets()` | `TogoIDConverter$new()$convert()`, `$get_ortholog()`, ... |
| `togoid_label2id()` | `LabelConverter$new()$convert()` |
| `togoid_annotate()`, `togoid_list_fields()` | `AnnotationsConverter$new()$execute_query()`, `$list_fields()` |

## Configuration

- `TOGOID_API_ENDPOINT`: TogoID API base URL (default: https://api.togoid.dbcls.jp)
- `TOGOID_GRASP_ENDPOINT`: GRASP GraphQL endpoint used for annotations (default: https://dx.dbcls.jp/grasp-dev-togoid)

```r
# Wrapper functions read the environment variables
Sys.setenv(TOGOID_API_ENDPOINT = "http://localhost:5000")

# R6 classes also take the URL directly
converter <- TogoIDConverter$new(api_base_url = "http://localhost:5000")
```

See [NEWS.md](NEWS.md) for changes, including breaking ones.

## Examples

### Enrichment Analysis for scRNA-seq

An example of building on TogoID: a conversion route that ends in an annotation
dataset (e.g. `ncbigene → uniprot → reactome_pathway`) is used as a gene-set
library for over-representation analysis of single-cell clusters, with the
results drawn on a UMAP.

> **Note:** This is an experimental, illustrative extension, not part of TogoID's
> core functionality. It ships as the `togoid_gene_sets()`,
> `togoid_enrich_clusters()`, `togoid_plot_umap_enrichment()` and related
> functions, but their API may change.

- `system.file("examples/scRNAseq_enrichment", package = "togoid")`: a runnable
  pipeline on real data ([inst/examples/scRNAseq_enrichment/](inst/examples/scRNAseq_enrichment/))
- `vignette("enrichment", package = "togoid")`: the functions and the optional
  packages they need ([vignettes/enrichment.Rmd](vignettes/enrichment.Rmd))

## Requirements

- R >= 4.0.0
- R6, httr2, jsonlite, dplyr, tibble, cli, rlang

## Testing

```r
devtools::test()
devtools::check()
```

Tests that call the TogoID API are skipped on CRAN and when offline. To run them
locally:

```bash
NOT_CRAN=true Rscript -e 'devtools::test()'
```

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
