#!/usr/bin/env Rscript
#
# Step 1: cluster a PBMC scRNA-seq dataset and compute a UMAP embedding.
#
# This step uses Seurat only; TogoID enters the pipeline in step 3. The
# clustering produced here is reused unchanged by every later step, so that the
# UMAP shown next to the enrichment results is exactly the one the gene lists
# came from.
#
# Usage:
#   Rscript 01_clustering.R [source] [results-dir]
#
# `source` is either
#   pbmc4k            the 4k PBMC dataset from the TENxPBMCData package
#                     (the default), or
#   <10x-matrix-dir>  a directory holding barcodes/features/matrix files.
#
# The default needs no manual download and is versioned by Bioconductor, so the
# figures in this example are reproducible: anyone who runs the pipeline with no
# arguments starts from the same counts. Step 1 also writes those counts out in
# 10x format, so the Python version of this example can read the same input.

suppressPackageStartupMessages({
  library(Seurat)
  library(togoid)
})

args <- commandArgs(trailingOnly = TRUE)
source_arg <- if (length(args) >= 1) args[1] else "pbmc4k"
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

# Pull the counts from Bioconductor. A missing package stops the run rather than
# quietly substituting other data, so the counts are always the ones named here.
load_pbmc4k <- function() {
  for (pkg in c("TENxPBMCData", "SingleCellExperiment")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf(paste0(
        "package '%s' is required for the 'pbmc4k' source.\n",
        "  Install it with:  BiocManager::install(\"%s\")\n",
        "  Or pass the path to a 10x matrix directory instead."
      ), pkg, pkg), call. = FALSE)
    }
  }

  sce <- TENxPBMCData::TENxPBMCData("pbmc4k")
  counts <- SingleCellExperiment::counts(sce)

  # Rows arrive as Ensembl IDs and the columns are unnamed; step 3 hands gene
  # symbols to TogoID, so switch to the symbols the dataset ships. Taking
  # Symbol_TENx and filling its gaps with the Ensembl ID reproduces what
  # Read10X() does with the same data's features.tsv.
  symbols <- SummarizedExperiment::rowData(sce)$Symbol_TENx
  missing <- is.na(symbols) | symbols == ""
  symbols[missing] <- rownames(sce)[missing]
  rownames(counts) <- make.unique(as.character(symbols))
  colnames(counts) <- as.character(SummarizedExperiment::colData(sce)$Barcode)

  # The counts are an HDF5-backed DelayedMatrix; Seurat wants them in memory.
  methods::as(counts, "dgCMatrix")
}

# Write the counts back out in 10x format so the Python example can start from
# exactly these numbers; keeping both languages on one input is what makes the
# two sets of results comparable.
write_10x <- function(counts, directory) {
  dir.create(directory, showWarnings = FALSE, recursive = TRUE)
  gz <- function(name) gzfile(file.path(directory, name), open = "wt")

  connection <- gz("barcodes.tsv.gz")
  writeLines(colnames(counts), connection)
  close(connection)

  connection <- gz("features.tsv.gz")
  writeLines(
    paste(rownames(counts), rownames(counts), "Gene Expression", sep = "\t"),
    connection
  )
  close(connection)

  temporary <- tempfile(fileext = ".mtx")
  Matrix::writeMM(counts, temporary)
  connection <- gz("matrix.mtx.gz")
  writeLines(readLines(temporary), connection)
  close(connection)
  unlink(temporary)

  directory
}

if (identical(source_arg, "pbmc4k")) {
  cat("Loading pbmc4k through TENxPBMCData (cached by Bioconductor)\n")
  counts <- load_pbmc4k()
  matrix_dir <- write_10x(counts, file.path(results_dir, "01_matrix_10x"))
  cat(sprintf("  wrote the same counts in 10x format to %s\n", matrix_dir))
} else {
  cat(sprintf("Loading 10x data from %s\n", source_arg))
  counts <- Read10X(data.dir = source_arg)
}

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
