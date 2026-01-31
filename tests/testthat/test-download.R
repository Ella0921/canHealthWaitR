testthat::test_that("mw_download_table downloads and extracts expected files", {
  testthat::skip_if_not_installed("httr2")
  testthat::skip_if_not_installed("readr")
  testthat::skip_if_offline()
  
  # Use temp cache to avoid polluting user cache
  cache_dir <- file.path(tempdir(), "mw_cache_test")
  if (dir.exists(cache_dir)) unlink(cache_dir, recursive = TRUE)
  
  pid <- 13100961
  
  res <- mw_download_table(pid, language = "en", cache_dir = cache_dir, refresh = TRUE)
  
  testthat::expect_type(res, "list")
  testthat::expect_true(all(c("data_csv", "metadata_csv", "zip_path", "out_dir") %in% names(res)))
  
  testthat::expect_true(file.exists(res$zip_path))
  testthat::expect_true(dir.exists(res$out_dir))
  testthat::expect_true(file.exists(res$data_csv))
  testthat::expect_true(file.exists(res$metadata_csv))
  
  # sanity: files are non-empty
  testthat::expect_gt(file.info(res$data_csv)$size, 100)
  testthat::expect_gt(file.info(res$metadata_csv)$size, 100)
})

testthat::test_that("mw_download_all_tables returns one row per registry entry", {
  testthat::skip_if_not_installed("httr2")
  testthat::skip_if_not_installed("tibble")
  testthat::skip_if_offline()
  
  # if registry function isn't available for some reason, skip cleanly
  if (!exists("mw_registry")) testthat::skip("mw_registry() not available")
  
  cache_dir <- file.path(tempdir(), "mw_cache_test_all")
  if (dir.exists(cache_dir)) unlink(cache_dir, recursive = TRUE)
  
  reg <- mw_registry()
  testthat::expect_true(all(c("table_key", "product_id") %in% names(reg)))
  
  out <- mw_download_all_tables(language = "en", cache_dir = cache_dir, refresh = TRUE)
  
  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_true(all(c("table_key", "product_id", "data_csv", "metadata_csv") %in% names(out)))
  testthat::expect_equal(nrow(out), nrow(reg))
  
  # files exist for all rows
  testthat::expect_true(all(file.exists(out$data_csv)))
  testthat::expect_true(all(file.exists(out$metadata_csv)))
})

testthat::test_that("mw_list_downloads lists csv files under cache_dir", {
  testthat::skip_if_not_installed("tibble")
  
  cache_dir <- file.path(tempdir(), "mw_cache_test_list")
  if (dir.exists(cache_dir)) unlink(cache_dir, recursive = TRUE)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Create a dummy csv to verify listing works without internet
  f1 <- file.path(cache_dir, "dummy.csv")
  writeLines("a,b\n1,2\n", f1)
  
  lst <- mw_list_downloads(cache_dir = cache_dir)
  
  testthat::expect_s3_class(lst, "tbl_df")
  testthat::expect_true(all(c("path", "file") %in% names(lst)))
  testthat::expect_true(any(lst$file == "dummy.csv"))
})