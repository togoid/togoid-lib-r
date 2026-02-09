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
  route = c("ncbigene", "ensembl_gene"),
  format = "dataframe"
)
print(result)
#   source_id      target_id
# 1         1 ENSG00000121410
# 2         9 ENSG00000171428

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
result <- togoid_label2id(
  labels = c("BRCA1", "TP53", "EGFR"),
  dataset = "ncbigene",
  taxonomy = "9606"  # Human
)

# Extract IDs
ids <- sapply(result, function(x) x$identifier)
print(ids)
# [1] "672"  "7157" "1956"
```

### Get Annotations

```r
# List available fields
fields <- togoid_list_fields("ncbigene")
print(fields)

# Get annotations
annotations <- togoid_annotate(
  dataset = "ncbigene",
  ids = c("672", "7157"),
  fields = c("label", "gene_synonym", "full_name")
)

# Access annotations
print(annotations[["672"]]$label)
# [1] "BRCA1"
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

result <- gene_symbols |>
  togoid_label2id(dataset = "ncbigene", taxonomy = "9606") |>
  lapply(function(x) x$identifier) |>
  unlist() |>
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
      res[[1]]$identifier
    })
  )
```

## API Reference

### Main Classes

#### TogoIDConverter

- `convert(ids, route, format, ...)` - Convert IDs between databases
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

#### AnnotationsConverter

- `get_dataset(dataset_name)` - Get dataset configuration
- `list_fields(dataset_name)` - List available annotation fields
- `execute_query(dataset_name, ids, fields, filters)` - Execute GraphQL query
- `build_rows(dataset_label, fields, field_meta, records, filters, compact)` - Build table rows

#### LabelConverter

- `convert(labels, dataset, taxonomy, ...)` - Convert labels to IDs (auto-detects API)
- `convert_pubdictionaries(labels, dictionaries, tags, threshold, ...)` - Use PubDictionaries API
- `convert_sparqlist(labels, sparqlist, label_types, taxonomy)` - Use SPARQList API

### Wrapper Functions

- `togoid_convert(ids, route, format, ...)` - ID conversion
- `togoid_get_ortholog(ids, route, target_taxids, format)` - Ortholog retrieval
- `togoid_annotate(dataset, ids, fields, filters)` - Get annotations
- `togoid_list_fields(dataset)` - List annotation fields
- `togoid_label2id(labels, dataset, taxonomy, ...)` - Label to ID conversion
- `togoid_search_databases(name)` - Search databases
- `togoid_route(src, dst, max_hops)` - Find routes

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

## Testing

```r
# Run tests
devtools::test()

# Check package
devtools::check()
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
