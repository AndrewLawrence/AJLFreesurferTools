
# Import, process the original DKT.csv file containing scoring coefficients
#   and logic. Taken from supplement of Potvin et al 2017
#
# Citation:
#   Potvin et al 2017 Neuroimage 2017 Aug 1;156:43-64.
#     doi: 10.1016/j.neuroimage.2017.04.035.           #nolint
#     https://pubmed.ncbi.nlm.nih.gov/28479474/


proc_dkt_csv <- function(f) {
  norms <- utils::read.csv(f)

  # For now, don't work with logs:
  norms <- norms[!grepl("_log$", norms$region), ]
  # TODO: make this work with logs.

  # Separate out hemisphere and measure information (init):
  norms$region_hemi <- NA
  norms$region_meas <- NA

  # Decode region names with _{L|R}{S|T|V} suffix:
  norms$region_meas[grepl("_.S$", norms$region)] <- "area"
  norms$region_meas[grepl("_.T$", norms$region)] <- "thickness"
  norms$region_meas[grepl("_.V$", norms$region)] <- "volume"

  norms$region_hemi[grepl("_L.$", norms$region)] <- "lh"
  norms$region_hemi[grepl("_R.$", norms$region)] <- "rh"

  # Decode hemi prefix from subcortical regions:
  norms$region_hemi[grepl("^Right_", norms$region)] <- "rh"
  norms$region_hemi[grepl("^Left_", norms$region)] <- "lh"

  # Remove decoded info from variables:
  norms$region <- gsub("_L.$", "", norms$region)
  norms$region <- gsub("_R.$", "", norms$region)
  norms$region <- gsub("^Left_", "", norms$region)
  norms$region <- gsub("^Right_", "", norms$region)

  # Lowercase, remove underscores:
  norms$region <- tolower(norms$region)
  norms$region <- gsub("_", "", norms$region)

  # add back in the hemi / measure:
  norms$region <- ifelse(
    is.na(norms$region_hemi),
    norms$region,
    paste0(norms$region_hemi, "_", norms$region)
  )
  norms$region <- ifelse(
    is.na(norms$region_meas),
    norms$region,
    paste0(norms$region, "_", norms$region_meas)
  )

  # Add measurement type to whitelisted subcortical regions:
  norms$region <- gsub("accumbensarea", "accumbensarea_vol", norms$region)
  norms$region <- gsub("amygdala", "amygdala_vol", norms$region)
  norms$region <- gsub("caudate", "caudate_vol", norms$region)
  norms$region <- gsub("hippocampus", "hippocampus_vol", norms$region)
  norms$region <- gsub("pallidum", "pallidum_vol", norms$region)
  norms$region <- gsub("putamen", "putamen_vol", norms$region)
  norms$region <- gsub("thalamusproper", "thalamus_vol", norms$region)

  # adjustments to syntax and subsitution in operant column:
  norms$operant <- paste0("", norms$name, " <- ", norms$operant)
  # modify:
  norms$operant <- gsub("P_[modality_manuf_id]", "P_mfr", norms$operant,
                        fixed = TRUE)
  norms$operant <- gsub("G_[modality_manuf_id]", "G_mfr", norms$operant,
                        fixed = TRUE)
  norms$operant <- gsub("[", "", norms$operant, fixed = TRUE)
  norms$operant <- gsub("]", "", norms$operant, fixed = TRUE)

  norms[, -which(colnames(norms) %in% c("region_hemi", "region_meas"))]
}


dkt_norms <- proc_dkt_csv("data-raw/DKT.csv")

usethis::use_data(dkt_norms, overwrite = TRUE, internal = TRUE)
