% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/importing_stats.R
\name{atlasstats_to_wide}
\alias{atlasstats_to_wide}
\title{atlasstats_to_wide}
\usage{
atlasstats_to_wide(
  x,
  is_aseg = FALSE,
  is_wm = FALSE,
  is_lgi = FALSE,
  hemi_label = ""
)
}
\arguments{
\item{x}{A named list of 2 data.frames "header" "dat" (from read_stats_data).}

\item{is_aseg}{Different behaviour if input is aseg rather than a
cortical atlas.}

\item{is_wm}{Different behaviour if input is a wm.aparc.stats rather than a
cortical atlas.}

\item{is_lgi}{Different behaviour if input is an lgi stats file rather than
a cortical atlas.}

\item{hemi_label}{A prefix for the output names (e.g. "lh", "rh", "bl").
Leave as "" for no prefix.}
}
\value{
A one row data.frame with \verb{$\{hemi\}_$\{roi\}_$\{meas\}} format column names.
}
\description{
Apply to the results of applying internal \code{read_statsdata} function to a
cortical atlas.
}
