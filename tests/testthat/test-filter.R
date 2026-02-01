testthat::test_that("mw_filter filters by geo/stat/year without breaking schema", {
  testthat::skip_if_not_installed("httr2")
  testthat::skip_if_not_installed("readr")
  testthat::skip_if_offline()
  
  df_raw <- mw_read_data(13100961)
  df_std <- mw_standardize(df_raw, table_id = "13100961")
  
  out <- mw_filter(
    df_std,
    geo = c("Canada", "British Columbia"),
    stat = c("Percent"),
    start_year = 2024,
    end_year = 2024
  )
  
  req <- c("table_id", "ref_date", "geo", "indicator", "stat", "value", "dims")
  testthat::expect_true(all(req %in% names(out)))
  
  # if non-empty, constraints must hold
  if (nrow(out) > 0) {
    testthat::expect_true(all(out$geo %in% c("Canada", "British Columbia")))
    testthat::expect_true(all(out$stat %in% "Percent"))
    testthat::expect_true(all(out$ref_date >= 2024 & out$ref_date <= 2024))
  }
})
