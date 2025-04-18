# Functions to work with Freesurfer "stats" files
#   (found in $SUBJECT_DIR/$SUBJECT/stats)
#
# Dev notes:
#   stats files include data for one parcellation (typically a single hemi)
#     from a single subject. The file has two components:
#     1) A data body in a table form with multiple values per region in the
#         parcellation
#     2) A header, commented out with '#' that contains metadata.
#
#   The internal function read_statsdata() reads the main data table
#
#   Three internal read_statsheader_XXX functions read different aspects
#     of the file header:
#
#     _colnames = simple vector of colnames (used in read_statsdata).
#
#     _measures = table of brain measures from the header, e.g. eTIV.
#                     (used in read_statsdata)
#
#     _tablecols = table of additional details for tabular data (e.g. units)
#                     (currently unused)
#
#   The user-visible (exported) functions:
#
#       readstats_subject
#         -and-
#       readstats_subjectlist
#
#     are used to import a particular parcellation for both hemispheres, and
#       the subcortical measures from an aseg, into a single row per subject
#       format.
#
#   Functions with the format rss_* are internal supporting functions for the
#     two readstats_subject* functions.
#

read_statsheader_colnames <- function(file) {
  x <- readLines(file)
  sel <- grep("^# ColHeaders", x)
  if ( length(sel) == 0L ) return(NULL)
  nm <- strsplit(x[sel], split = " ")[[1]]
  nm <- nm[nm != ""]
  nm[3:length(nm)]
}

read_statsheader_measures <- function(file,
                                      wide = FALSE) {
  x <- readLines(file)
  # header lines start with "# ":
  sel <- grep("^# Measure", x)
  if ( length(sel) == 0L ) {
    return(NA)
  }
  x <- x[sel]
  x <- gsub("# Measure ", "", x)
  x <- strsplit(x, split = ", ")
  x <- as.data.frame(do.call(rbind, x))
  colnames(x) <- c("rm", "name", "details", "value", "units")
  x$value <- as.numeric(x$value)
  x <- x[, -1]

  if ( ! wide ) {
    return(x)
  }
  setNames(x$value, x$name)
}


read_statsheader_tablecols <- function(file) {
  x <- readLines(file)
  # header lines start with "# ":
  sel <- grep("^# TableCol", x)
  if ( length(sel) == 0L ) {
    warning("no TableCol found")
    return(NULL)
  }
  x <- x[sel]
  x <- gsub("# TableCol ", "", x)
  # collapse consecutive spaces and trim whitespace:
  x <- trimws(gsub("[ ]+", " ", x))
  # split:
  x <- strsplit(x, split = " ")
  # assemble results:
  res <- data.frame(
    col_idx = sapply(x, \(k) k[[1]]),
    label = sapply(x, \(k) k[[2]]),
    contents = sapply(x, \(k) paste0(k[3:length(k)], collapse = " "))
  )

  res
}

#' @importFrom utils read.table
read_statsdata <- function(file) {
  hd <- read_statsheader_measures(file, wide = TRUE)
  dat <- read.table(
    file = file,
    sep = "",
    header = FALSE,
    comment.char = "#"
  )
  colnames(dat) <- read_statsheader_colnames(file)
  list(header = hd,
       data = dat)
}

#' atlasstats_to_wide
#'
#' Apply to the results of applying internal \code{read_statsdata} function to a
#'    cortical atlas.
#' @param x A named list of 2 data.frames "header" "dat" (from read_stats_data).
#' @param is_aseg Different behaviour if input is aseg rather than a
#'     cortical atlas.
#' @param is_wm Different behaviour if input is a wm.aparc.stats rather than a
#'     cortical atlas.
#' @param is_lgi Different behaviour if input is an lgi stats file rather than
#'     a cortical atlas.
#' @param hemi_label A prefix for the output names (e.g. "lh", "rh", "bl").
#'     Leave as "" for no prefix.
#' @return A one row data.frame with `${hemi}_${roi}_${meas}` format column names.
atlasstats_to_wide <- function(x,
                               is_aseg = FALSE,
                               is_wm = FALSE,
                               is_lgi = FALSE,
                               hemi_label = "") {

  hdr <- x$header
  if ( ! is_lgi ) {
    names(hdr) <- tolower(names(hdr))
  }

  x <- x$data

  # Add a delimiter to non-default hemi_label arguments:
  if ( hemi_label != "") {
    hemi_label <- paste0(hemi_label, "_")
  }

  # Ensure no underscores in roi names:
  snames <- gsub("_", "-", x$StructName)

  if ( is_wm ) {
    # wm.aparc specific processing:
    snames <- gsub("^wm-lh-", "lh_wm-", snames)
    snames <- gsub("^wm-rh-", "rh_wm-", snames)

    # then process as aseg:
    is_aseg <- TRUE
  }

  if ( is_aseg ) {
    # aseg specific set-up:
    hemi_label <- ""
    snames <- gsub("^Left-", "lh_", snames)
    snames <- gsub("^Right-", "rh_", snames)
    snames <- tolower(snames)
    snames <- gsub("\\.", "", snames)
    snames <- gsub("-", "", snames)

    snames <- ifelse(substr(snames, 1, 3) %in% c("lh_", "rh_"),
                     snames,
                     paste0("bl_", snames))

    # aseg specific header measure treatment:
    names(hdr) <- gsub("^lh", "lh_", names(hdr))
    names(hdr) <- gsub("^rh", "rh_", names(hdr))

    names(hdr) <- gsub("cortexvol", "cortex_volume", names(hdr))

    meas <- setNames(c("Volume_mm3"), c("vol"))
  } else {

    if ( is_lgi ) {
      meas <- setNames(c("Mean"), c("lgi"))
    } else {
      # cortical specific setup:
      meas <- setNames(c("SurfArea", "ThickAvg", "GrayVol"),
                       c("area", "thickness", "volume"))
      # For cortical surface segmentations,
      #   the first three elements of the header:
      #   NumVert, WhiteSurfArea, and MeanThickness
      #   refer *only* to the hemisphere of that cortical atlas.
      #   Remaining header measures are common / whole head measures
      #     and so are identical between lh.aparc.? and rh.aparc.?
      names(hdr)[1:3] <- paste0(hemi_label, tolower(names(hdr)[1:3]))
      # We want to change a couple of these names
      #     to keep a standard naming scheme:
      names(hdr) <- gsub("whitesurfarea", "cortex_area", names(hdr))
      names(hdr) <- gsub("meanthickness", "cortex_thickness", names(hdr))
    }

  }

  if ( ! is_lgi ) {
    result <- hdr
  } else {
    result <- NULL
  }

  for ( i in seq_along(meas) ) {
    nm <- paste0(hemi_label, snames, "_", names(meas)[[i]])
    result <- c(result, setNames(x[, meas[[i]]], nm))
  }
  result
}


rss_merge_unique_two <- function(a, b) {
  sel <- names(b)[names(b) %in% names(a)]
  chk <- vapply(X = sel,
                FUN = \(k) identical(unname(b[[k]]),
                                     unname(a[[k]])),
                FUN.VALUE = TRUE)
  if ( any(!chk) ) {
    stop("names may not be unique.")
  }
  c(a, b[! names(b) %in% names(a)])
}

rss_merge_unique <- function(...) {
  args <- list(...)
  n <- length(args)
  if (n < 2)
    return(args[[1]])
  if (n == 2)
    return(rss_merge_unique_two(args[[1]], args[[2]]))

  result <- args[[1]]

  for (i in 2:n) {
    result <- rss_merge_unique_two(result, args[[i]])
  }
  result
}


rss_validate_args <- function(s,
                              dir_path = NULL,
                              SUBJECTS_DIR = NULL, #nolint
                              read_aseg = TRUE) {

  if ( is.null(dir_path) && is.null(SUBJECTS_DIR) ) {
    SUBJECTS_DIR <- get_SUBJECTS_DIR() #nolint
  }

  if ( length(s) > 1 ) stop("Only one subject at a time.")
  if ( !is.null(dir_path) && length(dir_path) > 1 )
    stop("Only one subject at a time.")

  fpath <- paste0(SUBJECTS_DIR, "/", s, "/stats")

  if ( !is.null(dir_path) ) {
    fpath <- dir_path
  }

  # check specified directory exists:
  if ( !dir.exists(fpath) ) {
    stop(paste0("could not find: ", fpath))
  }

  if ( any(is.na(read_aseg)) || (length(read_aseg) != 1) ) {
    stop("could not interpret read_aseg as logical of length 1.")
  }

  fpath
}

rss_validate_files <- function(x) {
  for ( i in seq_along(x) ) {
    if ( !file.exists(x[i]) ) {
      stop(paste0("File not found: ", x[i]))
    }
  }
}

#' readstats_subject
#'
#' Read in a cortical atlas for a subject.
#'     Uses default naming convention for freesurfer to identify the requested
#'     atlas files and the aseg (if requested).
#'     Does not import wm.aparc as this is often not of interest and
#'     (by default settings) is only generated for the regions of the Desikan
#'     atlas.
#'
#' If requested (lgi = TRUE) then LGI files will be searched for with the
#'     naming convention: $hemi.$atlas.pial_lgi.stats
#'     in the subject/stats folder.
#'     e.g. lh.aparc.DKTatlas.pial_lgi.stats
#'
#' @return A one row data.frame containing stats values. Variable names follow
#'     hemi_region_measure format unless whole-brain measures. `bl_` indicates
#'     a region spanning left and right hemispheres.
#' @param s A subject ID. If a `dir_path` is set then this provides a label for
#'     the stats data dir_path points to. Otherwise `SUBJECTS_DIR` will be
#'     searched for the subject `s`.
#' @param dir_path Specify a path to a particular Freesurfer stats folder
#'     (e.g. "/data/study/fs/S01/stats")
#' @param SUBJECTS_DIR Specify the Freesurfer SUBJECTS_DIR directory.
#'     SUBJECTS_DIR is ignored if `dir_path` is set.
#' @param atlas Which cortical parcellation atlas to read.
#' @param hemi Which hemisphere(s) to export stats from.
#' @param lgi bool. TRUE: look for additional lgi files for the specified atlas.
#' @param read_aseg bool. Read aseg stats as well as cortical data?
#' @examples
#' # If SUBJECTS_DIR is set as a system variable:
#' \dontrun{
#'   readstats_subject("S01")
#' }
#' # Specifying SUBJECTS_DIR yourself:
#' \dontrun{
#'   readstats_subject("S01", SUBJECTS_DIR = "/path/to/SUBJECTS_DIR")
#' }
#' # Providing a label and a path to a stats folder:
#' \dontrun{
#'   readstats_subject("S01", dir_path = "/path/to/SUBJECTS_DIR/S01/stats")
#' }
#' @importFrom stats setNames
#' @export
readstats_subject <- function(s,
                              dir_path = NULL,
                              SUBJECTS_DIR = get_SUBJECTS_DIR(), #nolint
                              atlas = c("DKT", "Desikan", "Destrieux"),
                              hemi = c("both", "lh", "rh"),
                              lgi = TRUE,
                              read_aseg = TRUE) {
  cl <- as.list(environment())

  # Arguments with defaults:
  atlas <- match.arg(atlas, c("DKT", "Desikan", "Destrieux"))
  hemi <- match.arg(hemi, c("both", "lh", "rh"))

  if ( hemi == "both" ) {
    hemi <- c("lh", "rh")
  }

  read_aseg <- as.logical(read_aseg)

  fpath <- do.call(rss_validate_args,
                   cl[names(cl) %in% c("s", "dir_path",
                                       "SUBJECTS_DIR", "read_aseg")])

  # map atlas name to expected filename suffixes:
  alut <- c(DKT = "aparc.DKTatlas.stats",
            Desikan = "aparc.stats",
            Destrieux = "aparc.a2009s.stats")

  flist <- setNames(paste0(fpath, "/", hemi, ".", alut[atlas]), hemi)
  aseg_path <- paste0(fpath, "/aseg.stats")

  if ( read_aseg ) {
    rss_validate_files(c(flist, aseg_path))
  } else {
    rss_validate_files(flist)
  }


  fdat <- do.call(rss_merge_unique,
                  unname(mapply(\(f, lab) {
                    atlasstats_to_wide(read_statsdata(f),
                                       hemi_label = lab)
                  },
                  flist,
                  names(flist),
                  SIMPLIFY = FALSE)))

  if ( read_aseg ) {
    fdat <- rss_merge_unique_two(
      atlasstats_to_wide(read_statsdata(aseg_path), is_aseg = TRUE),
      fdat
    )
  }
  # convert fdat to a data.frame &
  #   add the subject id as the first column:
  fdat <- do.call(data.frame, c(list(fsid = s), as.list(fdat)))

  # lgi handling:
  if ( !lgi ) return(fdat)

  # Look for lgi files with expected names:
  lgi_alut <- c(DKT = "aparc.DKTatlas.pial_lgi.stats",
                Desikan = "aparc.pial_lgi.stats",
                Destrieux = "aparc.a2009s.pial_lgi.stats")

  flist <- setNames(paste0(fpath, "/", hemi, ".", lgi_alut[atlas]), hemi)
  # filter to exisiting files and if non return:
  flist <- flist[file.exists(flist)]
  if ( length(flist) < 1 ) return(fdat)

  # Otherwise read in and append lgi data:
  lgi <- do.call(rss_merge_unique,
                 unname(mapply(\(f, lab) {
                   atlasstats_to_wide(read_statsdata(f),
                                      is_lgi = TRUE,
                                      hemi_label = lab)
                 },
                 flist,
                 names(flist),
                 SIMPLIFY = FALSE)))

  do.call(data.frame, rss_merge_unique_two(fdat, lgi))
}

#' readstats_subjectlist
#'
#' Apply \code{\link{readstats_subject}} to **lots** of subject IDs and collate
#'    into a data.frame.
#'    Has two modes of operation: `SUBJECTS_DIR` and `dir_path`.
#'    `dir_path` is a list of direct paths to freesurfer subject folders, it is
#'    paired with the vector `s` which is used to label the subjects in
#'    `dir_path`.
#'    `SUBJECTS_DIR` is a directory containing all your subject folders, if
#'    this is set then `s` is a vector of subjects that exist within this
#'    directory.
#'    *Use `dir_path` if you don't have a typical freesurfer setup.
#'    If all subjects are in a single directory use `SUBJECTS_DIR`.*
#'
#' @param dir_path a vector of full paths to freesurfer directories
#' @param s a vector of subject IDs for subjects in `SUBJECTS_DIR`
#'     (alternatively labels for subjects if using `dir_path`).
#'     Default `s` will use \code{\link{list_fs_subjects}} to read all
#'     valid freesurfer subjects from `SUBJECTS_DIR` environment variable.
#' @inheritParams readstats_subject
#' @return Given *n* subject IDs returns a *n* row data.frame containing stats
#'     values. Variable names follow hemi_region_measure format unless
#'     whole-brain measures. `bl_` indicates a region spanning left and
#'     right hemispheres.
#' @examples
#' \dontrun{
#' ## SUBJECTS_DIR mode:
#' readstats_subjectlist(s = c("s1", "s2", "s3"),
#' SUBJECTS_DIR = "/path/to/SUBJECTS")
#' ## dir_path mode:
#' readstats_subjectlist(s = c("s1", "s2", "s3"),
#' dir_path = c("path/to/s1", "path/to/s2", "path/to/s3"))
#' }
#' @seealso readstats_subject
#' @export
readstats_subjectlist <- function(s = list_fs_subjects(),
                                  dir_path = NULL,
                                  SUBJECTS_DIR = get_SUBJECTS_DIR(), #nolint
                                  atlas = c("DKT", "Desikan", "Destrieux"),
                                  hemi = c("both", "lh", "rh"),
                                  lgi = TRUE,
                                  read_aseg = TRUE) {
  cl <- as.list(environment())
  cl$s <- NULL
  cl$dir_path <- NULL

  cat("reading stats for: ", length(s), " subjects\n")

  if ( !is.null(dir_path) ) {

    if ( length(dir_path) != length(s) )
      stop("labels (s) and dir_paths must be same length")

    rlist <- mapply(\(subj, dir) {
      do.call("readstats_subject",
              c(list(s = subj, dir_path = dir), cl))
    },
    s,
    dir_path,
    SIMPLIFY = FALSE)
  } else {
    rlist <- lapply(s,
                    \(subj) do.call("readstats_subject", c(list(s = subj), cl)))
  }

  if ( length(rlist) == 1L ) return(rlist[[1]])

  result <- rlist[[1]]

  for ( i in 2:length(rlist) ) {
    result <- merge(result, rlist[[i]], all = TRUE)
  }
  result
}
