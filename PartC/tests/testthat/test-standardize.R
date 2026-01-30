test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("mw_standardize renames and converts types correctly", {
  # 模拟 API 可能返回的各种列名组合
  df <- data.frame(GEO = "BC", REF_DATE = "2020-01", VALUE = "5.5", indicator_name = "test")
  
  result <- mw_standardize(df)
  
  # 验证结果
  expect_s3_class(result, "tbl_df")
  expect_equal(colnames(result)[1], "province") # 验证重命名
  expect_equal(result$year[1], 2020)            # 验证年份提取
  expect_type(result$val, "double")             # 验证数值转换
})