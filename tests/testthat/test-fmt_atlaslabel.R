test_that("fsprettify works", {
  # "test" should not be altered by shorten option:
  expect_equal(fmt_atlaslabel(c("test", "Fusiform")), c("test", "FFG"))
  # removing hemi information will collapse levels:
  expect_equal(fmt_atlaslabel(c("rh_fusiform", "lh_fusiform"),
                              remove_hemi = TRUE),
               c("FFG", "FFG"))
})
