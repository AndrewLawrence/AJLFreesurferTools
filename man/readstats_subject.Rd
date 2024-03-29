% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/importing_stats.R
\name{readstats_subject}
\alias{readstats_subject}
\title{readstats_subject}
\usage{
readstats_subject(
  s,
  dir_path = NULL,
  SUBJECTS_DIR = get_SUBJECTS_DIR(),
  atlas = c("DKT", "Desikan", "Destrieux"),
  hemi = c("both", "lh", "rh"),
  lgi = TRUE,
  read_aseg = TRUE
)
}
\arguments{
\item{s}{A subject ID. If a \code{dir_path} is set then this provides a label for
the stats data dir_path points to. Otherwise \code{SUBJECTS_DIR} will be
searched for the subject \code{s}.}

\item{dir_path}{Specify a path to a particular Freesurfer stats folder
(e.g. "/data/study/fs/S01/stats")}

\item{SUBJECTS_DIR}{Specify the Freesurfer SUBJECTS_DIR directory.
SUBJECTS_DIR is ignored if \code{dir_path} is set.}

\item{atlas}{Which cortical parcellation atlas to read.}

\item{hemi}{Which hemisphere(s) to export stats from.}

\item{lgi}{bool. TRUE: look for additional lgi files for the specified atlas.}

\item{read_aseg}{bool. Read aseg stats as well as cortical data?}
}
\value{
A one row data.frame containing stats values. Variable names follow
hemi_region_measure format unless whole-brain measures. \code{bl_} indicates
a region spanning left and right hemispheres.
}
\description{
Read in a cortical atlas for a subject.
Uses default naming convention for freesurfer to identify the requested
atlas files and the aseg (if requested).
Does not import wm.aparc as this is often not of interest and
(by default settings) is only generated for the regions of the Desikan
atlas.
}
\details{
If requested (lgi = TRUE) then LGI files will be searched for with the
naming convention: $hemi.$atlas.pial_lgi.stats
in the subject/stats folder.
e.g. lh.aparc.DKTatlas.pial_lgi.stats
}
\examples{
# If SUBJECTS_DIR is set as a system variable:
\dontrun{
  readstats_subject("S01")
}
# Specifying SUBJECTS_DIR yourself:
\dontrun{
  readstats_subject("S01", SUBJECTS_DIR = "/path/to/SUBJECTS_DIR")
}
# Providing a label and a path to a stats folder:
\dontrun{
  readstats_subject("S01", dir_path = "/path/to/SUBJECTS_DIR/S01/stats")
}
}
