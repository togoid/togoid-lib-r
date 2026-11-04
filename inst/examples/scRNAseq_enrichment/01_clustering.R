#!/usr/bin/env Rscript
#
# Step 1: cluster a 10x Genomics scRNA-seq dataset and compute a UMAP embedding.
#
# This step uses Seurat only; TogoID enters the pipeline in step 3. The
# clustering produced here is reused unchanged by every later step, so that the
# UMAP shown next to the enrichment results is exactly the one the gene lists
# came from.
#
# Usage:
#   Rscript 01_clustering.R <10x-matrix-dir> [results-dir]

suppressPackageStartupMessages({
  library(Seurat)
  library(togoid)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  stop("Usage: Rscript 01_clustering.R <10x-matrix-dir> [results-dir]", call. = FALSE)
}

data_dir <- args[1]
results_dir <- if (length(args) >= 2) args[2] else "results"

# Quality-control thresholds, matching the Python version of this example.
MIN_GENES <- 200
MAX_GENES <- 6000
MAX_MT_PERCENT <- 15
MIN_CELLS <- 3
N_PCS <- 30
RESOLUTION <- 0.5
SEED <- 0

dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

cat(strrep("=", 60), "\n")
cat("Step 1: clustering and UMAP\n")
cat(strrep("=", 60), "\n")

cat(sprintf("Loading 10x data from %s\n", data_dir))
counts <- Read10X(data.dir = data_dir)
object <- CreateSeuratObject(counts = counts, min.cells = MIN_CELLS,
                             min.features = MIN_GENES)
cat(sprintf("  loaded %d cells x %d genes\n", ncol(object), nrow(object)))

# Quality control.
object[["percent.mt"]] <- PercentageFeatureSet(object, pattern = "^MT-")
object <- subset(
  object,
  subset = nFeature_RNA > MIN_GENES &
    nFeature_RNA < MAX_GENES &
    percent.mt < MAX_MT_PERCENT
)
cat(sprintf("  after QC: %d cells x %d genes\n", ncol(object), nrow(object)))

# Normalisation, embedding and clustering.
object <- NormalizeData(object, verbose = FALSE)
object <- FindVariableFeatures(object, nfeatures = 2000, verbose = FALSE)
object <- ScaleData(object, verbose = FALSE)
object <- RunPCA(object, npcs = N_PCS, seed.use = SEED, verbose = FALSE)
object <- FindNeighbors(object, dims = seq_len(N_PCS), verbose = FALSE)
object <- FindClusters(object, resolution = RESOLUTION, random.seed = SEED,
                       verbose = FALSE)
object <- RunUMAP(object, dims = seq_len(N_PCS), seed.use = SEED, verbose = FALSE)

counts_per_cluster <- table(Idents(object))
cat(sprintf("  found %d clusters\n", length(counts_per_cluster)))
for (cluster in names(counts_per_cluster)) {
  cat(sprintf("    cluster %s: %d cells\n", cluster, counts_per_cluster[[cluster]]))
}

# The Seurat object, for step 2 and any further analysis.
saveRDS(object, file.path(results_dir, "01_clustered.rds"))
cat(sprintf("  wrote %s\n", file.path(results_dir, "01_clustered.rds")))

# A plain embedding table, which is all step 4 needs. Writing it keeps the later
# steps usable without Seurat.
embedding <- togoid_umap_from_seurat(object)
write.csv(embedding, file.path(results_dir, "01_umap.csv"), row.names = TRUE)
cat(sprintf("  wrote %s\n", file.path(results_dir, "01_umap.csv")))

cat("\nNext: 02_find_markers.R\n")
