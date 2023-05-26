
test_that("agrees with values in Discussion of Potvin 2017", {
  # "For example, using formulas for the whole left cortical hemisphere,
  #   a 66 year old female with an eTIV of 1528670 mm3 (estimation from
  #   FreeSurfer) scanned on a Siemens 3T would be expected to have..."
  #   a lh surface area of 80100 mm2,
  #   a lh thickness of 2.20 mm,
  #   and a lh volume of 193589 mm3
  #
  #   Zscores −0.28, −1.98, and −1.78

  value1 <- norm_a_region(
    value = 80100,
    region = "lh_cortex_area",
    participant_age = 66,
    gender = 0L,
    eTIV = 1528670,
    magnetic_field_strength = 0L,
    modality_manuf_id = "Siemens"
  )
  expect_equal(round(value1, 2), -0.28)

  value2 <- norm_a_region(
    value = 2.20,
    region = "lh_cortex_thickness",
    participant_age = 66,
    gender = 0L,
    eTIV = 1528670,
    magnetic_field_strength = 0L,
    modality_manuf_id = "Siemens"
  )
  expect_equal(round(value2, 2), -1.98)

  value3 <- norm_a_region(
    value = 193589,
    region = "lh_cortex_volume",
    participant_age = 66,
    gender = 0L,
    eTIV = 1528670,
    magnetic_field_strength = 0L,
    modality_manuf_id = "Siemens"
  )
  expect_equal(round(value3, 2), -1.78)
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
