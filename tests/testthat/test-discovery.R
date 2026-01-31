

test_that("mw_list_tables returns the registry tibble", {
  out <- mw_list_tables()
  reg <- mw_registry()
  
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("table_key", "product_id", "title"))
  
  # should match registry exactly (same rows/columns)
  expect_identical(out, reg)
})


test_that("mw_resolve_table resolves by table_key", {
  out <- mw_resolve_table("wait_time")
  
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1)
  expect_named(out, c("table_key", "product_id", "title"))
  
  expect_equal(out$table_key[[1]], "wait_time")
  expect_true(is.integer(out$product_id))
})


test_that("mw_resolve_table resolves by product_id", {
  reg <- mw_registry()
  pid <- reg$product_id[[1]]  # take a known valid id from registry
  
  out <- mw_resolve_table(pid)
  
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1)
  expect_equal(out$product_id[[1]], pid)
})


test_that("mw_resolve_table errors for unknown identifier", {
  expect_error(
    mw_resolve_table("not_a_real_key"),
    "Unknown table identifier",
    fixed = TRUE
  )
  
  expect_error(
    mw_resolve_table(99999999),
    "Unknown table identifier",
    fixed = TRUE
  )
})


test_that("mw_search_tables returns matches and keeps columns", {
  out <- mw_search_tables("health")
  
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("table_key", "product_id", "title"))
  
  # should return 1+ rows if registry has 'health' in titles
  expect_true(nrow(out) >= 1)
  
  # all returned titles should contain the query (case-insensitive)
  expect_true(all(grepl("health", tolower(out$title))))
})


test_that("mw_search_tables errors if query is not single string", {
  expect_error(mw_search_tables(123))
  expect_error(mw_search_tables(c("a", "b")))
})
