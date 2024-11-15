

test_that(
  "list_subjects_works",
  {
    loc <- withr::local_tempdir()
    withr::local_dir(loc)

    withr::local_envvar(.new = c("SUBJECTS_DIR" = getwd()))

    # set-up a fake freesurfer directory structure.
    ctrl <- make_dummy_subjectsdir()

    # make_dummy_subjectsdir created 32 example directories.
    expect_equal(nrow(ctrl), 32L)

    # type-specific lists:
    list_all <- list_fs_subjects(type = "all")
    list_cross <- list_fs_subjects(type = "cross")
    list_long <- list_fs_subjects(type = "long")
    list_base <- list_fs_subjects(type = "base")
    list_longbase <- list_fs_subjects(type = "long", include_base = TRUE)

    # expected (from ctrl):
    exp_all <- ctrl[ctrl$isDone, "x"]
    exp_cross <- ctrl[ctrl$isDone & ctrl$type == "cross", "x"]
    exp_long <- ctrl[ctrl$isDone & ctrl$type == "long", "x"]
    exp_base <- ctrl[ctrl$isDone & ctrl$type == "base", "x"]
    exp_longbase <- ctrl[ctrl$isDone & ctrl$type %in% c("base", "long"), "x"]

    # non-type specific:
    list_everything <- list_fs_subjects(type = "all", only_done = FALSE)
    list_everything_and_fsav <- list_fs_subjects(type = "all",
                                                 only_done = FALSE,
                                                 include_fsaverage = TRUE)

    # type specific checks:
    expect_setequal(list_all, exp_all)
    expect_setequal(list_cross, exp_cross)
    expect_setequal(list_base, exp_base)
    expect_setequal(list_long, exp_long)
    expect_setequal(list_longbase, exp_longbase)

    # fsaverage checks:
    expect_false("fsaverage" %in% c(list_everything, list_all, list_cross,
                                    list_long, list_base, list_longbase))
    expect_true("fsaverage" %in% list_everything_and_fsav)
  }
)
