
# AJLFreesurferTools

This R package is a collection of utility functions for working with [Freesurfer](https://surfer.nmr.mgh.harvard.edu/).

The package started for personal use and so functions may not be well documented 
or very robust.

Please use these functions with care and verify they do what you expect!

## Installation

You can install AJLFreesurferTools from github using the `remotes` package:

``` r
# install.packages("remotes")
remotes::install_github("AndewLawrence/AJLFreesurferTools")
```

## Example

Read in a subject's DKT cortical atlas statistics as a one-row data.frame:

``` r
library(AJLFreesurferTools)
readstats_subject(
  "bert",
  atlas = "DKT",
  SUBJECTS_DIR = "path/to/freesurfer/SUBJECTS_DIR"
)
```

## Related R packages

Check out Tim Schäfer's excellent
[fsbrain](https://github.com/dfsp-spirit/fsbrain) package, 
and supporting
[freesurferformats](https://github.com/dfsp-spirit/freesurferformats) package.

There is also the [ggseg](https://ggseg.github.io/ggseg/index.html)
package which works with and plots freesurfer cortical parcellation data.

Some functions in this package partly duplicate functionality from [ggseg](https://ggseg.github.io/ggseg/index.html).

This is for two reasons:

1) It can be difficult to install ggseg without admin rights 
(e.g. on a HPC server) as ggseg has sf (and consequently the C++ GDAL library)
as a hard dependency.
2) ggseg omits measures in the stats file header like lh_MeanThickness


## Naming Freesurfer stats variables

With `readstats_subject` and related functions I have tried 
(with minimal validation beyond the use cases I have for the data)
to "fix" Freesurfer's variable names. I can't claim that they are better in
general, but they follow some principles:

 * Ideally comprise 3 elements in fixed order separated by underscores:
    1) hemispheric scope (lh_, rh_, bl_)
    2) A region name
    3) A measure name (e.g. thickness, area, volume)
 * Format: \$\{hemisphere-indicator\}\_\$\{region-indicator\}\_\$\{measure-indicator\}
 * Example: lh_amygdala_volume
 * Manipulations required:
     * All StructNames are converted to lower case
     * All StructNames have underscores (\_) converted to hyphens (\-)
 * Exceptions:
     * Hemispheric scope is omitted for certain whole-brain measures, typically
         those from the measures contained in the header of the aseg files.
         Where regions span the midline this is indicated by "bl_" for "bilateral".
     * Measure is omitted when it is an integral part of the measure name. 
         e.g. CerebralWhiteMatterVol.
         I didn't want to edit variable names excessively.
     * White Matter volumes from wm.aparc have RegionNames with a wm- prefix.
         
The idea is this scheme supports:

  1) Use of \_ as a delimiter to derive sufficient hemi, region, and measure
     information for onwards analysis from just the variable name.
     e.g. with tidyverse's `tidyr::separate()` or `base::strsplit()`
  2) Use of a regex pattern for three strings separated by underscores for 
     identifying analysable data for a regional analysis (v.s. whole brain 
     measures which are typically covariates).
  3) The indicator for data relating only to the left hemisphere is consistently
      "^lh_", not sometimes "^lh.", sometimes "^lh_", and sometimes "Left-".
