# Functions to work with Freesurfer "stats" files
#   (found in $SUBJECT_DIR/$SUBJECT/stats)

get_stats_colnames <- function(file) {
  x <- readLines(file)
  sel <- grep("^# ColHeaders", x)
  if ( length(sel) == 0L ) { return(NULL) }
  nm <- strsplit(x[sel], split = " ")[[1]]
  nm <- nm[nm != ""]
  nm[3:length(nm)]
}

get_stats_header_measures <- function(file) {
  x <- readLines(file)
  # header lines start with "# ":
  sel <- grep("^# Measure", x)
  if ( length(sel) == 0L ) {
    warning("no measures found")
    return(NULL)
  }
  x <- x[sel]
  x <- gsub("# Measure ", "", x)
  x <- strsplit(x, split = ", ")
  x <- as.data.frame(do.call(rbind, x))
  colnames(x) <- c("rm", "name", "details", "value", "units")
  x$value <- as.numeric(x$value)
  x[,-1]
}


get_stats_tablecols <- function(file) {
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


get_stats_data <- function(file) {
  dat <- read.table(
    file = file,
    sep = "",
    header = FALSE,
    comment.char = "#"
  )
  colnames(dat) <- get_stats_colnames(file)
  dat
}