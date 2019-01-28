

# The primary input to Qdec is a text file, named qdec.table.dat,
#   containing the subject IDs, and discrete (categorical) and
#   continuous factors, in table format.
# This is essentially a table of demographics for your subjects
#   including all the variables and factors that you wish to consider.
#   You may have different discrete (categorical) factor names and levels
#   (or even no discrete factors,
#   in which case all column data are assumed to be continuous factors).
# If you want to control the order the factors appear in the display:
#   For each discrete (categorical) factor, there could exist a file named
#     <factor>.levels
#   which lists all possible levels.
#   These ... can be used to control the order different factors are displayed.
# For organizational purposes it is best to make a directory called qdec
#   within your $SUBJECTS_DIR.
# You can save the qdec.table.dat file in there.
# When Qdec runs it will also save your analyses to this directory.



# Function for writing out a flat tab separate text file, ready to send to unix:
.flat_writer <- function(x, file, col.names = F, row.names = F, ...) {
  A <- file(file, open = "wb") # binary mode to get rid of windows EOLs.
  utils::write.table(x, A,
              row.names = row.names, col.names = col.names,
              quote = F, eol = "\n", sep = "\t", ...)
  close(A)
}


#' convert_df_to_qdec
#'
#' This function converts a data.frame with an indexing variable of freesurfer
#'     subject IDs (\code{fsvar}) into a format ready for qdec.
#'     It will optionally write out
#'
#' @param data object coercible to \code{\link{data.frame}}
#' @param outroot path + root to output files: \itemize{
#'     \item{\code{outroot.table.dat} - table directly input to QDEC}
#'     \item{\code{factor.levels} - factor level labels (one per factor)}
#'     }
#'     If missing (NA) then nothing is written.
#' @param fsvar The variable in \code{data} containing the freesurfer id codes
#'     (cooercible to case-correct character)
#' @param factor.levels Should factor.levels label files be written.
#' @return a data.frame for inspection
#'     (function is primarily used for it's side effect of writing out files)
#' @examples
#' df <- data.frame(fsid = c("s1", "s2", "s3"),
#'     A = 1:3,
#'     B = letters[1:3],
#'     C = as.factor(letters[4:6]),
#'     D = as.ordered(letters[7:9]))
#' convert_df_to_qdec(df, fsvar = "fsid")
#' \dontrun{
#' convert_df_to_qdec(df, fsvar = "fsid", outroot = "example_qdec_table")
#' }
#' @export
convert_df_to_qdec <- function(data,
                               fsvar = "fsid",
                               outroot = NA,
                               factor.levels = T) {
  data <- as.data.frame(data)

  if ( !fsvar %in% names(data) ) stop(paste0(fsvar, " not found in data"))

  fsid <- data[, names(data) %in% fsvar]
  data <- data[, !names(data) %in% fsvar]

  # first ordered factors get converted to numeric:
  ords <- sapply(data, is.ordered)

  if ( any(ords) ) {
    data[, which(ords)] <- sapply(data[, which(ords), drop = F],
                                  function(x) as.numeric(x))
  }

  # second make a list of factors and levels:
  f <- NA
  facs <- !sapply(data, is.numeric)

  if ( any(facs) ) {

    flist <- data[, facs, drop = F]
    f <- lapply(flist, function(x) levels(as.factor(x)))
    names(f) <- paste(names(f), "levels", sep = ".")

  }

  # third assemble data.frame:
  R <- data.frame(fsid = fsid, data)
  if ( !is.na(outroot) ) {
    dir.create(outroot, showWarnings = F)
    .flat_writer(R,
                 file = paste0(outroot, "/", outroot, ".table.dat"),
                 col.names = T)
    if ( factor.levels & !identical(f,NA) ) {
      mapply(.flat_writer,
             x = f,
             file = paste0(outroot, "/", names(f)),
             col.names = F)
    }
  }
  return(list(data = R, factors = f))
}
