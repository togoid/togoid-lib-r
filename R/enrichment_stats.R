#' Statistics for over-representation analysis
#'
#' Base R already provides an exact hypergeometric distribution and the
#' Benjamini-Hochberg correction, so these helpers are thin wrappers that fix
#' the parameterisation used throughout the enrichment code and keep it
#' readable.
#'
#' @name enrichment-stats
#' @keywords internal
NULL

#' Hypergeometric over-representation p-value
#'
#' The one-sided upper-tail probability `P(X >= k)`, i.e. the probability of
#' seeing an overlap at least as large as the observed one by chance. This is
#' the same as a one-sided Fisher's exact test.
#'
#' @param k Observed overlap (query genes that are in the term).
#' @param background_size Total number of genes in the universe.
#' @param term_size Number of background genes annotated with the term.
#' @param query_size Number of query genes present in the background.
#'
#' @return Numeric vector of p-values in `[0, 1]`.
#' @export
#'
#' @examples
#' # 5 of a 20-gene query fall in a 50-gene term, out of 2000 genes
#' togoid_hypergeometric_pvalue(5, 2000, 50, 20)
togoid_hypergeometric_pvalue <- function(k, background_size, term_size, query_size) {
  # phyper(q, m, n, k) with lower.tail = FALSE gives P(X > q), so q = k - 1
  # yields P(X >= k). m is the term size, n the rest of the background, and k
  # (phyper's) the number drawn, i.e. the query size.
  p <- stats::phyper(
    q = k - 1,
    m = term_size,
    n = background_size - term_size,
    k = query_size,
    lower.tail = FALSE
  )
  # An overlap of zero or less is always at least as likely as observed.
  p[k <= 0] <- 1
  pmin(pmax(p, 0), 1)
}

#' Benjamini-Hochberg false discovery rate
#'
#' @param pvalues Numeric vector of raw p-values.
#'
#' @return Numeric vector of adjusted p-values, in the input order.
#' @export
#'
#' @examples
#' togoid_fdr(c(0.01, 0.04, 0.03, 0.005, 0.2))
togoid_fdr <- function(pvalues) {
  if (length(pvalues) == 0) {
    return(numeric(0))
  }
  stats::p.adjust(pvalues, method = "BH")
}

#' Fold enrichment
#'
#' Observed overlap divided by the overlap expected under independence.
#'
#' @param k Observed overlap.
#' @param background_size Universe size.
#' @param term_size Term size.
#' @param query_size Query size.
#'
#' @return Numeric vector of fold-enrichment values; 0 where the expected
#'   overlap would be zero.
#' @export
#'
#' @examples
#' togoid_fold_enrichment(20, 1000, 100, 100)
togoid_fold_enrichment <- function(k, background_size, term_size, query_size) {
  expected <- (query_size * term_size) / background_size
  ifelse(expected > 0, k / expected, 0)
}
