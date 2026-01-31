test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("mw_filter correctly subsets data based on parameters", {
  # 1. 准备标准化的模拟数据
  test_df <- tibble::tibble(
    province = c("British Columbia", "Alberta", "Ontario"),
    year = c(2020, 2021, 2022),
    indicator = c("Wait time for specialist", "Surgery wait", "Wait time for MRI"),
    val = c(10, 20, 30)
  )
  
  # 2. 测试省份筛选
  res_prov <- mw_filter(test_df, province = "Alberta")
  expect_equal(nrow(res_prov), 1)
  expect_equal(res_prov$province[1], "Alberta")
  
  # 3. 测试年份范围筛选
  res_year <- mw_filter(test_df, start_year = 2021)
  expect_equal(nrow(res_year), 2)
  expect_true(all(res_year$year >= 2021))
  
  # 4. 测试关键词模糊匹配 (grepl)
  res_ind <- mw_filter(test_df, indicator = "specialist")
  expect_equal(nrow(res_ind), 1)
  expect_match(res_ind$indicator[1], "specialist", ignore.case = TRUE)
})