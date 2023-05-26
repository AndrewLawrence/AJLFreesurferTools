
# pow(x,y) used instead of x^y in the logic of the operant column of normstab:
pow <- function(x, y) {
  x^y
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
#'     a particular expected format. Default uses internal dkt_norms data.frame
#'     adapted from the DKT.csv file supplied in the supplementary material of
#'     Potvin et al 2017.
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
