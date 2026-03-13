.extract_stats_vars <- function(x, prefix = "lh_", suffix = "_lgi") {
  prefix <- paste0("^", prefix)
  suffix <- paste0(suffix, "$")

  grep(suffix, grep(prefix, colnames(x), value = TRUE), value = TRUE)
}

.make_default_lgi_roi_list <- function(x) {
  # x is a dataset made by readstats_subject(list)
  lh_lgi_vars <- .extract_stats_vars(x, "lh_", "_lgi")
  rh_lgi_vars <- .extract_stats_vars(x, "rh_", "_lgi")

  lh_area_vars <- gsub("_lgi", "_area", lh_lgi_vars)
  rh_area_vars <- gsub("_lgi", "_area", rh_lgi_vars)

  lh_valid <- lh_area_vars %in% colnames(x)
  rh_valid <- rh_area_vars %in% colnames(x)

  list(
    lh_cortex_lgi = list(values = lh_lgi_vars[lh_valid],
                         weights = lh_area_vars[lh_valid]),
    rh_cortex_lgi = list(values = rh_lgi_vars[rh_valid],
                         weights = rh_area_vars[rh_valid])
  )
}

.report_lgi_roi_list <- function(x) {
  # x is a lgi_roi_list.
  # run after checking.
  # Will print to console the number of regions per hemisphere.
  measures <- names(x)
  mlengths <- sapply(x, \(xx) length(xx[[1]]))
  strings <- paste0("Measure ",
                    measures,
                    " takes average over ",
                    mlengths,
                    " columns.\n")

  cat("---\n")
  cat("Weighted average calculations\n")
  cat("---\n")
  for ( s in strings ) {
    cat(s)
  }
  cat("---\n")
  cat("\n")
}


.check_lgi_roi_list <- function(x, cols) {
  chk <- lapply(x, \(aa) {
    lapply(aa, \(bb) {
      all(bb %in% cols)
    })
  })
  if (!all(unlist(chk))) {
    stop("lgi_roi_list contains column names not found in dataset.")
  }

  # check equal length within each hemi:
  chk2 <- lapply(x, \(aa) do.call("==", unname(lapply(aa, length))))
  if (!all(unlist(chk2))) {
    stop("lgi_roi_list contains a hemisphere with mismatched values/weights")
  }

  NULL
}

#' calculate_average_lgi_stats
#'
#' Calculates a per-hemisphere weighted average of lgi.
#'  Unless `lgi_roi_list` is specified, this function expects a
#'  column naming system consistent with output from
#'  \code{\link{readstats_subject}}.
#'  The default behaviour is to weight LGI by cortical surface area, but
#'  the lgi_roi_list structure can be used to calculate generic weighted
#'  averages within x.
#'
#' @param x a dataframe output by \code{\link{readstats_subject}}
#' @param lgi_roi_list a named list of lists containing paired vectors, e.g.:
#' ```
#'  list(
#'    lh_cortex_lgi = list(values = c("lh_roi1_lgi", "lh_roi2_lgi", ...),
#'                        weights = c("lh_roi1_area", "lg_roi2_area", ...),
#'    rh_cortex_lgi = list(values = c("rh_roi1_lgi", "rh_roi2_lgi", ...),
#'                        weights = c("rh_roi1_area", "rh_roi2_area", ...),
#'  )
#'  ```
#'    Each vector must contain column names from `x`.
#' The default (`NULL`) attempts to determine these automatically by simple
#'   matching of prefix and suffix using typical naming rules.
#' The names the list elements are used as the new variable names for the
#'  calculated weighted averages.
#' @return A data.frame with rows corresponding to `x` where columns contain the
#'     weighted averages specified in `lgi_roi_list`. Default: lh_cortex_lgi
#'     and rh_cortex_lgi.
#' @importFrom stats weighted.mean
#' @export
calculate_average_lgi_stats <- function(x,
                                        lgi_roi_list = NULL) {
  if ( is.null(lgi_roi_list) ) {
    lgi_roi_list <- .make_default_lgi_roi_list(x)
  }

  # error if check fails:
  .check_lgi_roi_list(lgi_roi_list, cols = colnames(x))
  # report to console:
  .report_lgi_roi_list(lgi_roi_list)

  res <- matrix(NA, nrow = nrow(x), ncol = length(lgi_roi_list))
  colnames(res) <- names(lgi_roi_list)
  rownames(res) <- rownames(x)

  for ( i in seq_len(nrow(res)) ) {
    for ( j in seq_len(ncol(res)) ) {
      res[i, j] <- weighted.mean(x = unlist(x[i, lgi_roi_list[[j]]$values]),
                                 w = unlist(x[i, lgi_roi_list[[j]]$weights]),
                                 na.rm = TRUE)
    }
  }
  res[is.nan(res)] <- NA

  res <- as.data.frame(res)
  res
}
