testthat::test_that("mw_standardize produces plot-ready schema for all target tables", {
  ids <- c("13100961", "13100962", "41100081")
  
  for (id in ids) {
    df_raw <- mw_read_data(as.numeric(id))
    df_std <- mw_standardize(df_raw, table_id = id)
    
    # required columns
    req <- c("table_id", "ref_date", "geo", "indicator", "stat", "value", "dims")
    testthat::expect_true(all(req %in% names(df_std)))
    
    # types
    testthat::expect_true(is.character(df_std$table_id))
    testthat::expect_true(is.numeric(df_std$ref_date) || is.integer(df_std$ref_date))
    testthat::expect_true(is.character(df_std$geo))
    testthat::expect_true(is.character(df_std$indicator))
    testthat::expect_true(is.character(df_std$stat))
    testthat::expect_true(is.numeric(df_std$value))
    testthat::expect_true(is.list(df_std$dims))
    
    # basic sanity (not all NA)
    testthat::expect_true(any(!is.na(df_std$ref_date)))
    testthat::expect_true(any(nzchar(df_std$geo)))
    testthat::expect_true(any(nzchar(df_std$indicator)))
    testthat::expect_true(any(nzchar(df_std$stat)))
  }
})