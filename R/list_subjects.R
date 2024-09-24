
# Internal function to obtain SUBJECTS_DIR from the system environment variables
get_SUBJECTS_DIR <- function() { #nolint
  s <- Sys.getenv("SUBJECTS_DIR")
  if ( identical(s, "") ) {
    s <- NULL
  }
  s
}

chk_dirpath_is_freesurfer <- function(x) {
  # We count anything where a /mri/orig sub-directory has been generated.
  #   this follows logic in the listsubj script.
  dir.exists(paste0(x, "/mri/orig"))
}

chk_is_cross <- function(x) {
  # Only cross-sectional processed data has an 001.mgz file in the
  #   mri/orig/ folder #nolint
  file.exists(paste0(x, "/mri/orig/001.mgz"))
}
chk_is_base <- function(x) {
  file.exists(paste0(x, "/base-tps"))
}
chk_is_long <- function(x) {
  # Note: here we deviate from listsubj which only checks the directory name
  #       for the string "*.long.*". Potentially someone might rename the
  #       directories, so instead we look for the argument "-long"
  #       within the call to recon-all (saved in recon-all.log).
  ra <- paste0(x, "/scripts/recon-all.log")
  res <- logical(length = length(ra))
  for ( i in seq_along(res) ) {
    shiboleth <- scan(ra[i], what = "",
                      nmax = 1, skip = 3, sep = "\n", quiet = TRUE)
    res[i] <- grepl("-long", shiboleth)
  }
  res
}

chk_is_error <- function(x) {
  file.exists(paste0(x, "/scripts/recon-all.error"))
}
chk_is_running <- function(x) {
  file.exists(paste0(x, "/scripts/IsRunning.lh")) |
    file.exists(paste0(x, "/scripts/IsRunning.rh"))
}
chk_is_done <- function(x) {
  file.exists(paste0(x, "/scripts/recon-all.done"))
}


#' list_fs_subjects
#'
#' This function produces a list of valid fsids within a given directory.
#' In the process it assesses *types* of data
#' (cross-sectional, longitudinal, base) and different processing
#' statuses (done, running, error). It can filter the returned values on
#' the basis of type and status.
#'
#' `list_fs_subjects` is essentially an R-version of the bash script `listsubj`
#' that is a part of Freesurfer distributions.
#'
#' Given the path to a directory containing processed Freesurfer data,
#'     return a list of the directory names (i.e. fsid) for valid data.
#'
#' @param SUBJECTS_DIR The directory to search. If `NULL`, the function will
#'    attempt to read the environment variable: `${SUBJECTS_DIR}`.
#' @param type Should output be a particular processing type to return:
#'
#'    | cross | cross-sectional processed data |
#'    | ----- | ------------------------------ |
#'    | long  | longitudinal processed data    |
#'    | base  | mid-space ("base") templates from longitudinal processing |
#'    | all   | all the above                  |
#' @param detailed Logical. If `TRUE` return a data.frame with participant
#'     id, directory path, and subject type information.
#' @param include_base The default behaviour (`"auto"`) is that base
#'      templates are included for `type = "all"` but not for `type = "cross"`
#'      or `type = "long"`. This can be overridden by setting to `TRUE` or
#'      `FALSE` to always include templates (if present), or never include them.
#' @param only_done Logical. If `TRUE` directories without a recon-all.done file
#'     are omitted
#' @param include_fsaverage Logical. Should `list_fs_subjects` include the
#'     *fsaverage* default directory in its return value (if present).
#' @return A vector of Freesurfer IDs found within `SUBJECTS_DIR` of the
#'     appropriate `type`. Or - if `detailed = TRUE` - a `data.frame`.
#' @examples
#' \dontrun{
#'
#' # Note: subject directory environment variable can be (re-)set from within R:
#' Sys.setenv(SUBJECTS_DIR = "/path/to/SUBJECTS_DIR")
#'
#' # Vector of all "done" freesurfer directories:
#' list_fs_subjects(type = "all")
#' # Vector of "done" cross-sectional directories:
#' list_fs_subjects(type = "cross")
#' # Vector of "done" longitudinally processed directories:
#' list_fs_subjects(type = "long")
#' # Vector of "done" longitudinal base templates:
#' list_fs_subjects(type = "base")
#' # Vector of "done" longitudinal data and any "done" base templates:
#' list_fs_subjects(type = "long", include_base = TRUE)
#'
#' # A data.frame with meta-data on type and status
#' dat <- list_fs_subjects(type = "all", detailed = TRUE, only_done = FALSE)
#' # Freesurfer directories with errors:
#' dat[dat$error_status,]
#' # Freesurfer directories still running:
#' dat[dat$running_status,]
#' }
#'
#' @export
list_fs_subjects <- function(
    SUBJECTS_DIR = get_SUBJECTS_DIR(), #nolint
    type = c("all", "cross", "long", "base"),
    detailed = FALSE,
    only_done = TRUE,
    include_base = "auto",
    include_fsaverage = FALSE
) {

  type <- match.arg(type, choices = c("all", "cross", "long", "base"))
  if ( include_base == "auto" ) {
    include_base <- type %in% c("all", "base")
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

  # Optionally exclude the fsaverage directory which is automatically
  #   linked by freesurfer
  if ( !include_fsaverage ) {
    dlist <- dlist[names(dlist) != "fsaverage"]
  }

  # Initial filter of directories:
  dlist <- dlist[chk_dirpath_is_freesurfer(dlist)]

  # Format as data.frame:
  dlist <- data.frame(id = names(dlist),
                      path = unname(dlist))

  # Extract type info:
  dlist$is_base <- chk_is_base(dlist$path)
  dlist$is_long <- chk_is_long(dlist$path)
  dlist$is_cross <- chk_is_cross(dlist$path)

  # Extract status info:
  dlist$error_status <- chk_is_error(dlist$path)
  dlist$running_status <- chk_is_running(dlist$path)
  dlist$done_status <- chk_is_done(dlist$path)

  if ( only_done ) {
    dlist <- dlist[dlist$done_status, ]
  }

  # Make a logical filter for the return:
  if ( type == "all" ) {
    inc <- rep(TRUE, nrow(dlist))
  }
  if ( type == "cross" ) {
    inc <- dlist$is_cross
  }
  if ( type == "long" ) {
    inc <- dlist$is_long
  }
  if ( type == "base" ) {
    inc <- dlist$is_base
  }

  if ( include_base ) {
    inc[dlist$is_base] <- TRUE
  }

  # Return either the data.frame or a vector:
  if ( detailed ) {
    return(dlist[inc, ])
  }
  dlist$id[inc]
}
