
# functions to run QC on Freesurfer stats (from readstats_subject)

# Suppress a R CMD CHECK note about undefined variables:
if (getRversion() >= "2.15.1")  utils::globalVariables(c("fsid"))

# Internal function for scaling a data.frame:
trimmed_scale_df <- function(x, trim = 0.2) {
  q1 <- sapply(x, \(k) quantile(k, trim / 2, na.rm = TRUE))
  q2 <- sapply(x, \(k) quantile(k, 1 - trim / 2, na.rm = TRUE))

  m <- mapply(\(k, low, high) mean(k[k > low & k < high], na.rm = TRUE),
              x,
              q1,
              q2)

  s <- mapply(\(k, low, high) sd(k[k > low & k < high], na.rm = TRUE),
              x,
              q1,
              q2)
  as.data.frame(scale(x, center = m, scale = s))
}

#' qc_prep
#'
#' Prepare a Freesurfer dataset (output by \code{\link{readstats_subjectlist}})
#'     for QC analysis.
#' This function:
#'  * Removes \code{fsid} from the data.frame and uses it for rownames
#'     (leaving numeric only variables)
#'  * Robustly scales (with \code{\link[base]{scale}}) all remaining variables
#'     using the trimmed mean and sd.
#'
#' @param x A data.frame from \code{\link{readstats_subjectlist}}
#' @param trim Remove this fraction of the data before calculating means/sds.
#'     Note that \code{0.5 * trim} is removed from each tail.
#' @export
#' @md
qc_prep <- function(x, trim = 0.2) {
  rownames(x) <- x$fsid
  x <- subset(x, select = -fsid) #nolint
  trimmed_scale_df(x, trim = trim)
}

#' qc_eTIV
#'
#' Run simple outlier detection on a single variable (designed for eTIV).
#'     Copies the basic logic from the \code{car} package's
#'     \code{\link[car]{outlierTest}} function, but applies adjustment for the
#'     False Discovery Rate using the B-H procedure instead of Bonferroni.
#'     The top 6 most significant results are printed to console.
#' @section Interpretation:
#'     I suggest carefully inspecting Talairach registrations for any subjects
#'     who meet \code{p_adj < 0.05}.
#'     Even when no subjects meet this threshold it is a good idea to note and
#'     perhaps inspect the subjects with the largest and smallest values of
#'     \code{rstudent} to check if results are reasonable.
#'
#' @param x A data.frame of Freesurfer stats with rownames set to fsid.
#'     (see \code{\link{readstats_subject}} and \code{\link{qc_prep}}).
#' @param etiv_lab The quoted column name of eTIV data within *x*
#' @return A data.frame containing studentised residuals, adjusted, and
#'     unadjusted p.values (sorted by p).
#' @importFrom stats pt df.residual rstudent lm
#' @importFrom stats as.formula p.adjust quantile sd
#' @importFrom utils head globalVariables
#' @export
qc_etiv <- function(x, etiv_lab = "eTIV") {
  f <- stats::as.formula(paste0(etiv_lab, " ~ 1"))
  # extract studentised residuals:
  m <- stats::lm(f, data = x)
  rs <- stats::rstudent(m)
  rs <- rs[!is.na(rs)]

  df <- stats::df.residual(m)

  p <- 2 * (stats::pt(abs(rs), df, lower.tail = FALSE))
  # adjust p for number of observations:
  p_adj <- stats::p.adjust(p, method = "fdr")

  result <- data.frame(rstudent = rs, p = p, p_adj = p_adj)[order(p), ]

  print(utils::head(result))
  invisible(result)
}

#' qc_rstudent
#'
#' Obtain studentised residuals from a linear model of measure ~ eTIV
#'     with a robust M-estimator fit
#'     (employs \code{\link[MASS]{rlm}} with default settings).
#' @param x A data.frame of Freesurfer measures with fsid in the rownames.
#' @param etiv_lab The variable name for the eTIV control variable in \code{x}.
#'     All other variables in \code{x} will be adjusted for linear effects of
#'     \code{etiv_lab}.
#' @return A data.frame containing studentised residuals
#'     (see: \code{\link[stats]{rstudent}}).
#' @importFrom MASS rlm
#' @importFrom stats sd
#' @export
qc_rstudent <- function(x, etiv_lab = "eTIV") {
  nm <- colnames(x)
  nm <- nm[nm != etiv_lab]

  sel <- which(sapply(nm, \(k) stats::sd(x[[k]], na.rm = TRUE)) > 0)

  if ( any(!sel) ) {
    warning(paste0("Variables removed due to zero non-missing variability: ",
                   paste0(nm[!sel], collapse = ", ")))
  }

  # list of formulas:
  flist <- lapply(nm[sel], \(k) stats::as.formula(paste0(k, " ~ ", etiv_lab)) )

  # fit models:
  m <- lapply(flist, \(k) MASS::rlm(k, data = x))

  # Studentised residuals from the models:
  r <- do.call(cbind, lapply(m, rstudent))
  colnames(r) <- nm[sel]

  r
}
