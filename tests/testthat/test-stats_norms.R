test_that("single subject norm works", {
  value <- norm_a_region(
    value = 7000,
    region = "lh_lingual_volume",
    participant_age = 42,
    gender = 1L,
    eTIV = 1524783,
    magnetic_field_strength = 0L,
    modality_manuf_id = "GE"
  )
  # Validated against mmc1.xlsm in supplement:
  expect_equal(round(value, 2), 0.43)
})

test_that("multiple subjects error out correctly", {
  # Missing the default column names:
  expect_error(
    norm_statsdf(mtcars)
  )
  # Column names set correctly, but wrong manufacturer:
  expect_error(
    norm_statsdf(mtcars,
                 etiv_colname = "mpg",
                 age_colname = "disp",
                 gender_colname = "hp",
                 id_colname = "am",
                 modality_manuf_id = "not_a_scanner_manufacturer")
  )
})