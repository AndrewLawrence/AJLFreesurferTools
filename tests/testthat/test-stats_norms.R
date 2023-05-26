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
