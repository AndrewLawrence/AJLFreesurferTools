
get_SUBJECTS_DIR <- function() { #nolint
  s <- Sys.getenv("SUBJECTS_DIR")
  if ( identical(s, "") ) {
    s <- NULL
  }
  s
}

chk_dirpath_is_freesurfer <- function(x) {
  file.exists(paste0(x, "/scripts/recon-all.log"))
}

chk_is_template <- function(x) {
  file.exists(paste0(x, "/base-tps"))
}

chk_is_long <- function(x) {
  ra <- paste0(x, "/scripts/recon-all.log")
  res <- logical(length = length(ra))
  for ( i in seq_along(res) ) {
    shiboleth <- scan(ra[i], what = "",
                      nmax = 1, skip = 3, sep = "\n", quiet = TRUE)
    res[i] <- grepl("-long", shiboleth)
  }
  res
}


#' list_fs_subjects
#'
#' Given the path to a Freesurfer formatted subjects directory return a list of
#'     the valid subject directories found within.
#'
#' @param SUBJECTS_DIR The directory to search. If `NULL`, function will attempt
#'    to read the environment variable `${SUBJECTS_DIR}`.
#' @param type What types of subject directories to return:
#'    | type  | details |
#'    | ----  | ------- |
#'    | cross | cross-sectional subjects |
#'    | long  | longitudinal subjects    |
#'    | template | mid-space templates from longitudinal runs |
#'    | all | all the above |
#' @param detailed Logical. If `TRUE` return a data.frame with participant
#'     id, directory path, and subject type information.
#' @param include_longtemplates The default behaviour (`"auto"`) is that
#'      templates are included for `type = "all"` but not for `type = "cross"`
#'      or `type = "long"`. This can be overridden by setting to `TRUE` or
#'      `FALSE` to always include templates (if present), or never include them.
#'
#' @return a vector of Freesurfer IDs found within `SUBJECTS_DIR` of the
#'     appropriate `type`. If `detailed = TRUE` then the return value is a
#'     `data.frame` instead.
#' @export
list_fs_subjects <- function(
    SUBJECTS_DIR = get_SUBJECTS_DIR(), #nolint
    type = c("all", "cross", "long", "template"),
    detailed = FALSE,
    include_longtemplates = "auto"
) {

  type <- match.arg(type, choices = c("all", "cross", "long", "template"))
  if ( include_longtemplates == "auto" ) {
    include_longtemplates <- type == "all"
  }

  if ( is.null(SUBJECTS_DIR) ) {
    stop("SUBJECTS_DIR not set. Provide directory path.")
  }
  if ( !dir.exists(SUBJECTS_DIR) ) {
    stop("SUBJECTS_DIR directory not found.")
  }
  # master list of all sub-directories in SUBJECTS_DIR
  dlist <- list.dirs(path = SUBJECTS_DIR, full.names = TRUE, recursive = FALSE)
  dlist <- setNames(dlist, basename(dlist))

  has_reconall_log <- chk_dirpath_is_freesurfer(dlist)

  dlist <- dlist[has_reconall_log]

  dlist <- data.frame(id = names(dlist),
                      path = unname(dlist))

  dlist$is_template <- chk_is_template(dlist$path)
  dlist$is_long <- chk_is_long(dlist$path)
  dlist$is_cross <- !dlist$is_long & !dlist$is_template

  if ( type == "all" ) {
    inc <- rep(TRUE, nrow(dlist))
  }
  if ( type == "cross" ) {
    inc <- dlist$is_cross
  }
  if ( type == "long" ) {
    inc <- dlist$is_long
  }
  if ( include_longtemplates ) {
    inc[dlist$is_template] <- TRUE
  }

  if ( detailed ) {
    return(dlist[inc, ])
  }
  dlist$id[inc]
}
