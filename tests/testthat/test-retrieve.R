testthat::test_that("mw_read_data reads main CSV into a tibble", {
  testthat::skip_if_not_installed("readr")
  testthat::skip_if_not_installed("httr2")
  testthat::skip_if_offline()
  
  pid <- 13100961
  
  df <- mw_read_data(pid, language = "en")
  
  testthat::expect_s3_class(df, "tbl_df")
  # core columns should exist for StatCan table CSVs
  testthat::expect_true(all(c("REF_DATE", "GEO", "VALUE") %in% names(df)))
  testthat::expect_gt(nrow(df), 0)
})

testthat::test_that("mw_read_metadata reads MetaData CSV into a tibble", {
  testthat::skip_if_not_installed("readr")
  testthat::skip_if_not_installed("httr2")
  testthat::skip_if_offline()
  
  pid <- 13100961
  
  meta <- mw_read_metadata(pid, language = "en")
  
  testthat::expect_s3_class(meta, "tbl_df")
  testthat::expect_gt(nrow(meta), 0)
  # metadata file usually contains these fields (based on your printed output)
  testthat::expect_true(any(grepl("Cube Title", names(meta), fixed = TRUE)) || ncol(meta) > 1)
})