



# Function for writing a 'flat', newline-terminated, tab-separated, text file.
#   Ready to open with unix:
#' @importFrom utils write.table
flat_writer <- function(x, file, col_names = FALSE, row_names = FALSE, ...) {
  x <- as.data.frame(x)
  f <- file(file, open = "wb") # binary mode to get rid of windows EOLs.
  write.table(
    x,
    file = f,
    row.names = row_names,
    col.names = col_names,
    quote = FALSE,
    eol = "\n",
    sep = "\t",
    ...
  )
  close(f)
}

#' fmt_atlaslabel
#'
#' Common manipulations for freesurfer atlas labels: forces lower case then
#' optionally removes hemisphere, removes measure and shortens labels with
#' common abbreviations.
#'
#' This is a WIP developed/tested with the DKT atlas.
#'
#' This function does not check that the result has as many unique levels as the
#' input (sometimes this is the desired behaviour). When using this function,
#' (particularly with the shorten option) verify it hasn't collapsed levels
#' in a way you were not expecting.
#'
#' @param x object coercible to character
#' @param remove_hemi strip hemisphere from start ^lh_|^rh_
#' @param remove_measure strip measure from end _lgi$|_vol$|_thickness$|_area$
#' @param shorten make shorter labels via common anatomical abbreviations
#'     e.g. amyg = amygdala, OFC = orbitofrontal, medial = m
#' @return a character vector
#' @export
fmt_atlaslabel <- function(x,
                           remove_hemi = FALSE,
                           remove_measure = TRUE,
                           shorten = TRUE) {
  x <- tolower(as.character(x)) # coerce lowercase:

  if (remove_measure) {
    x <- gsub("_lgi$|_vol$|_thickness$|_area$", "", x)
  }
  if (remove_hemi) {
    x <- gsub("^lh_|^rh_", "", x)
  }
  if (shorten) {
    # pattern gets less specific, so specific cases handled first.
    ptn <- c(
      "cuneus" = "CUN",
      "hippocampal" = "HC",
      "hippocampus" = "HC",
      "caudate" = "CAUD",
      "insula" = "INS",
      "thalamusproper" = "THAL",
      "entorhinal" = "EC",
      "pallidum" = "PALL",
      "amygdala" = "AMYG",
      "putamen" = "PUTA",
      "accumbens" = "NA",
      "fusiform" = "FFG",
      "lingual" = "MOG",
      "posteriorcingulate" = "PCC",
      "anteriorcingulate" = "ACC",
      "isthmuscingulate" = "ICC",
      "middlefrontal" = "MFG",
      "orbitofrontal" = "OFC",
      "transversetemporal" = "TTG",
      "supramarginal" = "SMG",
      "superiorfrontal" = "SFG",
      "superiortemporal" = "STG",
      "superiorparietal" = "SPG",
      "middletemporal" = "MTG",
      "inferiortemporal" = "ITG",
      "inferiorparietal" = "IPG",
      "lateraloccipital" = "LOG",
      "parsopercularis" = "IFGoper",
      "parstriangularis" = "IFGtri",
      "parsorbitalis" = "IFGorb",
      "central" = "CEN",
      "calcarine" = "CAL",
      "rostral" = "r",
      "medial" = "m",
      "caudal" = "c",
      "lateral" = "l",
      "superior" = "s"
    )

    for (i in seq_along(ptn)) {
      x <- gsub(names(ptn)[i], ptn[i], x)
    }
  }
  return(x)
}


#' convert_df_to_qdec
#'
#' This function converts a \code{data.frame} with an indexing variable of
#'     freesurfer subject IDs (\code{fsvar}) into a format ready for
#'     \href{https://surfer.nmr.mgh.harvard.edu/fswiki/Qdec}{QDEC}.
#'     It will optionally write out factor.levels files and dummy code your
#'     factors.
#'
#' @param data object coercible to \code{\link{data.frame}}
#' @param outroot path + root to output files: \itemize{
#'     \item{\code{outroot.table.dat} - table directly input to QDEC}
#'     \item{\code{factor.levels} - factor level labels (one per factor)}
#'     }
#'     If missing (NA) then nothing is written.
#' @param fsvar character variable name in \code{data} containing
#'     the freesurfer id codes.
#' @param dummy_vars (character) names of variables which should be
#'     dummy coded (see below).
#' @param write_levels Should the factor.levels label files be written.
#' @return a data.frame for inspection.
#'    N.B. this function is primarily used for it's side-effect of
#'         writing the files required for QDEC.
#' @details \itemize{
#'     \item{factor.levels are used to order factor levels for display.}
#'     \item{QDEC's "Regression Coefficients" display option applies to the
#'         first non-\code{fsvar} variable.}}
#' @section dummy_vars: QDEC is limited in how it
#'     models factors (\code{mri_glmfit} is much better).
#'     QDEC *always* includes interactions between factors of interest
#'     and covariates, even if these may not be of interest.
#'     This is a "different-onset, different-slope" or
#'     \href{https://surfer.nmr.mgh.harvard.edu/fswiki/DodsDoss}{DODS} model
#'     As a result, some complex models cannot be fit in QDEC, for example
#'     when a nuisance factor has a large number of levels
#'     (e.g. MRI-Site effects).
#'     Factors can be dummy coded to get a Different Onset, Same Slope (DOSS)
#'     model (i.e. one which does not include the interaction terms)
#'     this will also work around factor limits.
#'     For a factor with k-levels k-1 dummy predictors are created, the first
#'     level of the factor is omitted as an implicit baseline/reference.
#'     Finally, within QDEC all the dummy factors must then be selected
#'     all together in 'Nuisance Factors'.
#' @examples
#' df <- data.frame(fsid = c("s1", "s2", "s3"), A = 1:3, B = letters[1:3],
#'                  C = as.factor(letters[4:6]), D = as.ordered(letters[7:9]))
#' convert_df_to_qdec(df, fsvar = "fsid", dummy_vars = "C")
#' \dontrun{
#' convert_df_to_qdec(df, fsvar = "fsid", dummy_vars = "C",
#'     outroot = "example_qdec_table")
#' }
#' @export
convert_df_to_qdec <- function(data,
                               fsvar = "fsid",
                               dummy_vars = NA,
                               outroot = NA,
                               write_levels = TRUE) {
  # The primary input to Qdec is a text file, named qdec.table.dat,
  #   containing the subject IDs, and discrete (categorical) and
  #   continuous factors, in table format.
  # This is essentially a table of demographics for your subjects
  #   including all the variables and factors that you wish to consider.
  #   You may have different discrete (categorical) factor names and levels
  #   (or even no discrete factors,
  #   in which case all column data are assumed to be continuous factors).

  data <- as.data.frame(data)
  if ( any(is.na(data)) ) {
    warning(paste(sum(is.na(data)),
                  "missing values found, affected cases removed"))
    data <- data[stats::complete.cases(data), ]
  }
  if ( !identical(length(fsvar), 1L) )
    stop("There can only be one Subject ID variable")
  if ( !fsvar %in% names(data) )
    stop(paste0(fsvar, " not found in data"))

  fsid <- data[, names(data) %in% fsvar]
  data <- data[, !names(data) %in% fsvar]

  # first: ordered factors get converted to numeric:
  ords <- sapply(data, is.ordered)
  if ( any(ords) ) {
    data[, which(ords)] <- sapply(data[, which(ords), drop = FALSE],
                                  function(x) as.numeric(x))
  }

  # second: dummy code factors if required:
  if ( !identical(dummy_vars, NA) && !all(dummy_vars %in% colnames(data)) ) {
    stop("all dummy_vars must be column names in data")
  }

  if ( !identical(dummy_vars, NA) ) {
    if ( any(sapply(data[, dummy_vars], is.numeric)) )
      stop("Dummy variables must be of type character or unordered factor")
    dummys <- data[, which(colnames(data) %in% dummy_vars), drop = FALSE]
    data <- data[, which(!colnames(data) %in% dummy_vars), drop = FALSE]
    dummys <- data.frame(stats::model.matrix(stats::as.formula(~ .), dummys))
    data <- data.frame(data, dummys[, -1])
  }

  # third: remaining discrete factors (and optionally their levels):
  f <- NA
  facs <- !sapply(data, is.numeric)

  if ( any(facs) ) {

    flist <- data[, facs, drop = FALSE]
    f <- lapply(flist, function(x) levels(as.factor(x)))
    names(f) <- paste(names(f), "levels", sep = ".")

  }

  # fourth: assemble data.frame and output
  r <- data.frame(fsid = fsid, data)

  if ( !is.na(outroot) ) {

    dir.create(outroot, showWarnings = FALSE)

    flat_writer(r,
                file = paste0(outroot, "/", outroot, ".table.dat"),
                col.names = TRUE)

    if ( write_levels && !identical(f, NA) ) {
      mapply(flat_writer,
             x = f,
             file = paste0(outroot, "/", names(f)),
             col.names = FALSE)
    }
  }

  return(list(data = r, factors = f))
}
