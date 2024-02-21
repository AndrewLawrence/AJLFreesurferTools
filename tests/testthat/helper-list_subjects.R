
make_dummy_fs_subject <- function(
  x,
  type = c("cross", "long", "base", "bad"),
  isDone = TRUE, #nolint
  isRunning = FALSE, #nolint
  isError = FALSE  #nolint
) {
  # in the current working directory will create a dummy fs subject
  #   x = a fsid (dir name)
  type <- match.arg(type, choices = c("cross", "long", "base", "bad"))

  dir.create(paste0(x, "/scripts"), recursive = TRUE, showWarnings = FALSE)
  dir.create(paste0(x, "/mri/orig"), recursive = TRUE, showWarnings = FALSE)

  if ( isDone ) {
    file.create(paste0(x, "/scripts/recon-all.done"))
  }
  if ( isError ) {
    file.create(paste0(x, "/scripts/recon-all.error"))
  }
  if ( isRunning ) {
    file.create(paste0(x, "/scripts/IsRunning.lh"))
  }

  if ( type == "long" ) {
    write.table(
      c("a", "b", "c", "test -long", "e"),
      col.names = FALSE,
      row.names = FALSE,
      file = paste0(x, "/scripts/recon-all.log")
    )
  } else {
    write.table(
      letters[1:5],
      col.names = FALSE,
      row.names = FALSE,
      file = paste0(x, "/scripts/recon-all.log")
    )
  }

  switch(type,
    cross = file.create(paste0(x, "/mri/orig/orig.mgz")),
    base = file.create(paste0(x, "/base-tps"))
  )

  invisible(NULL)
}

make_dummy_subjectsdir <- function() {
  # Note: bad type is not defined.
  ctrl <- expand.grid(type = c("cross", "long", "base", "bad"),
                      isDone = c(FALSE, TRUE),
                      isRunning = c(FALSE, TRUE),
                      isError = c(FALSE, TRUE),
                      stringsAsFactors = FALSE)

  # Hack some informative names:
  x <- paste(ctrl$type,
             c("", "NotDone")[1 + (1 - ctrl$isDone)],
             c("", "Running")[1 + ctrl$isRunning],
             c("", "Error")[1 + ctrl$isError],
             sep = "_")
  x <- gsub("__", "_", x)
  x <- gsub("__", "_", x)
  x <- gsub("_$", "", x)

  ctrl <- data.frame(x, ctrl)

  for ( i in seq.int(nrow(ctrl)) ) {
    args <- as.list(ctrl[i, ])
    do.call(make_dummy_fs_subject, args = args)
  }

  # also make a fake fsdir
  make_dummy_fs_subject("fsaverage", type = "cross", isDone = TRUE)

  ctrl
}
