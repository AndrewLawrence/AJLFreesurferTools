
# pow(x,y) used instead of x^y in the logic of the operant column of normstab:
pow <- function(x, y) {
  x^y
}

check_magnetic_field_strength <- function(x) {
  length(x) == 1L & (
    x == 1L | x == 0L
  )
}

#' norm_a_region
#'
#' Produce a Z-score for a single freesurfer parcellation measure in a single
#'     subject. Norms are based on Potvin et al 2017.
#'
#' @param value The value of an observed quantity (e.g. thickness, surfacearea)
#'     in a particular region.
#' @param region The name of the region in which the quantity was observed.
#'     (expects names from readstats_subject* functions,
#'     i.e. in \code{normstab$region}).
#' @param participant_age The age in years of the participant.
#'     Can be decimals (e.g. 21.6)
#' @param gender Dummy coded. Either 1L (for Male) or 0L (for Female).
#' @param eTIV Freesurfer's eTIV measurement in mm3 units (e.g. 1415670)
#' @param magnetic_field_strength Dummy coded.
#'     Either 1L (for 1.5T), or 0L (for 3T).
#' @param modality_manuf_id Name of MRI scanner's manufacturer. One of
#'     \code{c("Siemens","Phillips", "GE")}.
#' @param normstab A data.frame of regression coefficients and operations in
#'     a particular expected format. By default the internal data.frame
#'     \code{dkt_norms} is used. This was adapted from the DKT.csv file
#'     supplied in the supplementary material of Potvin et al 2017.
#'     This only works for the cortical DKT parcellation and aseg regions.
#' @return a single numeric value
#' @export
norm_a_region <- function(value,
                          region, # character, from normstab region column.
                          participant_age,
                          gender, # 1 = Male, 0 = Female
                          eTIV,   # mm3, i.e. should have 7 digits # nolint
                          magnetic_field_strength = 0L, # 1 = 1.5T, 0 = 3T
                          modality_manuf_id, # "Siemens","Phillips", "GE"
                          normstab = dkt_norms # in data-raw/dkt_norms.R
) {

  # infer atlas from region:
  sel <- which(normstab$region == region)
  atab <- unique(normstab[sel, c("region", "atlas_id")])

  if ( nrow(atab) != 1 ) {
    stop(paste0("Error, wrong number of matches for region:", region))
  }
  # filter to selected atlas:
  normstab <- normstab[normstab$atlas_id == atab$atlas_id, ]

  modality_manuf_id <- match.arg(modality_manuf_id,
                                 c("Siemens", "Phillips", "GE"))

  normcmd <- gsub("Z=", "", normstab[normstab$name == "NORME", "operant"])

  # check region:
  if ( ! region %in% normstab$region ) {
    stop(paste0("not found: ", region))
  }

  normdat <- normstab[normstab$name != "NORME" &
                        normstab$region %in% c("", region), ]


  # Make an environment containing the const values:
  env <- list2env(setNames(as.list(normdat$constant),
                           normdat$name))
  env$participant_age <- participant_age
  env$value <- value
  env$gender <- gender
  env$eTIV <- eTIV
  env$magnetic_field_strength <- magnetic_field_strength

  # special treatment for scanner manufacturer:
  P_mfr <- 0L # nolint
  G_mfr <- 0L # nolint
  if ( modality_manuf_id == "Philips" ) {
    P_mfr <- 1L # nolint
  }
  if ( modality_manuf_id == "GE" ) {
    G_mfr <- 1L # nolint
  }

  env$P_mfr <- P_mfr
  env$G_mfr <- G_mfr

  # evaluate the commands in the environment:
  for ( i in seq_along(normdat$operant)) {
    eval(parse(text = normdat$operant[i]), envir = env)
  }

  # evaluate the norm:
  z <- eval(parse(text = normcmd), envir = env)

  return(z)
}



#' norm_statsdf
#'
#' Produce Z-scores based on Potvin et al 2017's norming procedure for *all*
#'     freesurfer parcellation measures in a data.frame.
#'     See \code{\link{norm_a_region}}
#'     for key arguments. This function infers \code{value} and
#'     \code{region} from the data.frame column names and contents. It is
#'     designed to work with the output of \code{\link{readstats_subjectlist}}.
#'
#' This function assumes \code{statsdf} contains data from a single scanner,
#'    and so \code{magnetic_field_strength} and \code{modality_manuf_id} are
#'    specified once. Other non-region/value data required for norming
#'    must be included as columns in \code{statsdf}.
#'    These are: \code{participant_age} (in years),
#'    \code{gender} (0|1 = Female|Male),
#'    and \code{eTIV} (in mm^3).
#' @param statsdf a data.frame (probably derived from
#'     \code{\link{readstats_subjectlist}}).
#' @param etiv_colname The column name of the variable containing estimated
#'     Total Intracranial Volume (mm^3).
#' @param age_colname The column name of the variable containing participants
#'     age in years (must be numeric, can be decimal).
#' @param gender_colname The column name of the variable containing participants
#'     gender, coded as 0 for Female, and 1 for Male.
#' @param id_colname The column name#' containing the freesurfer ID.
#' @return A data.frame of z-scored data. Variable names from \code{statsdf}
#'     are prefixed by \code{z_}.
#' @inheritParams norm_a_region
#' @export
norm_statsdf <- function(statsdf,
                         etiv_colname = "etiv",
                         age_colname = "age",
                         gender_colname = "gender",
                         id_colname = "fsid",
                         magnetic_field_strength = 0L,
                         modality_manuf_id,
                         normstab = dkt_norms) {
  # Check input:
  if (!"data.frame" %in% class(statsdf)) {
    stop("statsdf must be a data.frame")
  }

  for (i in c(etiv_colname, age_colname, gender_colname, id_colname)) {
    if (!i %in% colnames(statsdf)) {
      stop(paste0("Column not found in statsdf: ", i))
    }
  }

  if (!check_magnetic_field_strength(magnetic_field_strength)) {
    stop("magnetic_field_strength not specified correctly")
  }

  modality_manuf_id <-
    match.arg(modality_manuf_id,
              choices = c("Siemens", "Phillips", "GE"))
  # END check input.

  # Identify suitable regions:
  rois <-
    colnames(statsdf)[colnames(statsdf) %in% normstab$region]


  res <- lapply(
    setNames(rois, rois),
    function(roi) {
      vapply(seq_len(nrow(statsdf)),
             function(i) {
               do.call(
                 what = norm_a_region,
                 args = list(
                   value = statsdf[i, roi],
                   region = roi,
                   participant_age = statsdf[i, age_colname],
                   gender = statsdf[i, gender_colname],
                   eTIV = statsdf[i, etiv_colname],
                   magnetic_field_strength = magnetic_field_strength,
                   modality_manuf_id = modality_manuf_id,
                   normstab = normstab
                 )
               )
             },
             FUN.VALUE = 42.0)
    }
  )

  res <- as.data.frame(res)
  colnames(res) <- paste0("z_", colnames(res))
  res <- cbind(tmp = statsdf[, id_colname], res)
  colnames(res)[1] <- id_colname
  res
}
