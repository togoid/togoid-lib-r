# scRNA-seq enrichment analysis with TogoID (R)

A complete worked example: cluster a single-cell RNA-seq dataset with Seurat,
convert each cluster's marker genes with TogoID, test them for enrichment, and
draw the result on the UMAP embedding.

The point of the example is step 3. One call takes gene symbols all the way to
an annotation database, and the *only* thing that changes between Reactome, GO
and MONDO is the route:

```r
togoid_gene_sets(genes, route = c("ncbigene", "uniprot", "reactome_pathway"))
togoid_gene_sets(genes, route = c("ncbigene", "uniprot", "go"))
togoid_gene_sets(genes, route = c("ncbigene", "medgen", "mondo"))
```

This is the R counterpart of the Python example in `togoid-lib-python`; both
produce the same numbers from the same gene lists.

## Data

The example uses the 10x Genomics public PBMC dataset. It is **not** included in
this repository; download it from 10x Genomics and note the licence terms on
their site.

```bash
mkdir -p data && cd data
# Pick any "Filtered feature-barcode matrix (MTX)" PBMC dataset from
# https://www.10xgenomics.com/datasets and unpack it here, so that you end up
# with a directory containing barcodes.tsv.gz, features.tsv.gz and matrix.mtx.gz
tar -xzf filtered_feature_bc_matrix.tar.gz
```

Any 10x-format directory works — only step 1 touches the raw data.

## Requirements

```r
install.packages(c("Seurat", "ggplot2", "patchwork"))
# plus togoid itself
```

Steps 3 and 4 alone need only `ggplot2` and `patchwork`; the enrichment analysis
itself needs nothing beyond the package's own imports.

## Running the pipeline

```bash
./run_pipeline.sh path/to/filtered_feature_bc_matrix results
```

or step by step:

```bash
Rscript 01_clustering.R     path/to/filtered_feature_bc_matrix results
Rscript 02_find_markers.R   results
Rscript 03_enrichment.R     results
Rscript 04_visualize_umap.R results 3
```

Steps 1 and 2 need Seurat; steps 3 and 4 do not, and read only the CSV files the
earlier steps wrote.

## What each step does

### 1. `01_clustering.R` — clustering and UMAP

Loads the 10x matrix, applies standard QC (200–6000 genes per cell, <15%
mitochondrial reads), normalises, runs PCA, builds the neighbour graph, computes
the UMAP and clusters with Louvain.

**Outputs**

| File | Contents |
|---|---|
| `01_clustered.rds` | The Seurat object, for step 2 and further analysis |
| `01_umap.csv` | `umap_1, umap_2, cluster` — all step 4 needs |

The clustering is computed once here and reused unchanged by every later step,
so the UMAP shown beside the enrichment results is exactly the one the gene
lists came from.

### 2. `02_find_markers.R` — marker genes

`FindAllMarkers()` with a Wilcoxon test, filtered by adjusted p-value and log
fold change.

**Outputs**

| File | Contents |
|---|---|
| `02_markers.csv` | Long format `cluster, gene` — the input to step 3 |
| `02_marker_stats.csv` | The full `FindAllMarkers()` table |
| `02_marker_gene_lists/cluster_*_markers.txt` | One plain gene list per cluster |

### 3. `03_enrichment.R` — TogoID conversion and enrichment

For each target database, `togoid_gene_sets()` resolves the gene symbols to NCBI
Gene IDs, walks the route, fetches the term labels, and returns a gene-set
library. `togoid_enrich_clusters()` then runs a hypergeometric test per cluster
with BH-FDR correction, against a background of every marker gene in the
experiment.

| Target | Route | Term sizes |
|---|---|---|
| `reactome` | `ncbigene → uniprot → reactome_pathway` | 5–500 |
| `go` | `ncbigene → uniprot → go` (biological process) | 5–500 |
| `mondo` | `ncbigene → medgen → mondo` | 3–500 |

**Outputs** (per target)

| File | Contents |
|---|---|
| `03_genesets_<target>.json` | The gene-set library, cached |
| `03_enrichment_<target>_all.csv` | Every tested term |
| `03_enrichment_<target>_significant.csv` | FDR < 0.05 only |
| `03_enrichment_<target>_summary.txt` | Readable per-cluster report |
| `03_enrichment_<target>.rds` | Both objects, for further work in R |

Run a subset with `Rscript 03_enrichment.R results reactome,go`. To add another
database, add an entry to `targets` at the top of the script with its route —
nothing else changes.

The saved JSON is interchangeable with the Python library's
`GeneSetLibrary.save_json()`, so a library built in one language can be reused
in the other.

### 4. `04_visualize_umap.R` — UMAP figures

Draws a two-panel figure per target: cluster UMAP on the left, and the same
embedding on the right with each cluster's enriched terms written around its
centroid. Font size scales with `-log10(p)`, and labels that cannot be placed
without overlapping are dropped rather than drawn illegibly.

**Outputs**

| File | Contents |
|---|---|
| `04_umap_centroids.pdf/.png` | Reference figure showing where labels are anchored |
| `04_umap_enrichment_<target>_top<N>.pdf/.png` | The two-panel enrichment figure |
| `04_umap_enrichment_<target>_top<N>.tsv` | The terms drawn on that figure, one row per term |
| `04_umap_enrichment_<target>_top<N>_by_cluster.tsv` | The same terms, one row per cluster |

The TSV tables are written with the same filters the figure used, so the two can
never disagree. Tabs rather than commas, because term labels contain commas.

Change the number of terms per cluster with
`Rscript 04_visualize_umap.R results 5`, and hide the centroid markers with a
fourth argument: `Rscript 04_visualize_umap.R results 3 reactome,go,mondo FALSE`.

## Reading the results

- **Background choice matters.** The default background is every marker gene in
  the experiment, not the whole genome. This asks "which terms distinguish this
  cluster from the other clusters?", which is usually the question you want for
  cell types. Pass `background =` to `togoid_enrich_clusters()` for a
  genome-wide universe instead.
- **Ribosomal genes dominate some clusters.** In PBMC data, highly expressed
  ribosomal protein genes often appear in several clusters' marker lists, so
  translation pathways come out strongly. Either raise the log fold change
  threshold in step 2 or filter `RPL*`/`RPS*` out of the gene lists if you want
  cell-type-specific biology only.
- **MONDO sets are small.** Disease annotations cover far fewer genes than
  pathways, so step 3 uses a lower minimum term size for MONDO, and fewer terms
  reach significance.

## Doing this without the scripts

The whole analysis is four calls:

```r
library(togoid)

markers <- togoid_markers_from_seurat(Seurat::FindAllMarkers(object, only.pos = TRUE))
gene_sets <- togoid_gene_sets(
  sort(unique(unlist(markers, use.names = FALSE))),
  route = c("ncbigene", "uniprot", "reactome_pathway")
)
results <- togoid_enrich_clusters(markers, gene_sets)

figure <- togoid_plot_umap_enrichment(
  togoid_umap_from_seurat(object), results, top_n = 3
)
ggplot2::ggsave("umap_enrichment.pdf", figure, width = 20, height = 8)
```

See `vignette("enrichment", package = "togoid")` for the full manual.

## Licence note

The 10x Genomics PBMC data is distributed by 10x Genomics under their own terms.
Check the licence on the dataset page before redistributing it or the results
derived from it. Nothing from that dataset is included in this repository.
