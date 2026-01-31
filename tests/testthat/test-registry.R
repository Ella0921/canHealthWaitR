

test_that("mw_registry returns correct structure", {
  reg <- mw_registry()
  
  # Check tibble class
  expect_s3_class(reg, "tbl_df")
  
  # Check column names
  expect_named(reg, c("table_key", "product_id", "title"))
  
  # Check row count
  expect_equal(nrow(reg), 3)
  
  # Check column types
  expect_true(is.character(reg$table_key))
  expect_true(is.integer(reg$product_id))
  expect_true(is.character(reg$title))
})


test_that("mw_registry contains expected values", {
  reg <- mw_registry()
  
  # Check expected table keys exist
  expect_true(all(
    c("wait_time", "health_indicators", "fn_access") %in% reg$table_key
  ))
  
  # No missing product IDs
  expect_false(any(is.na(reg$product_id)))
  
  # Keys are unique
  expect_equal(length(unique(reg$table_key)), nrow(reg))
  
  # Product IDs are unique
  expect_equal(length(unique(reg$product_id)), nrow(reg))
})


test_that("mw_cache_dir returns stable cache directory path", {
  p1 <- mw_cache_dir()
  p2 <- mw_cache_dir()
  
  # Should be character scalar
  expect_type(p1, "character")
  expect_length(p1, 1)
  
  # Should be stable across calls
  expect_identical(p1, p2)
  
  # Should contain medwait cache folder
  expect_true(grepl("medwait", p1))
  
  # Should end with statcan_wds folder
  expect_true(grepl("statcan_wds$", p1))
})
