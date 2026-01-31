test_that("mw_plot_trend returns a ggplot object", {
  df <- tibble::tibble(
    ref_date = c(2020L, 2021L, 2022L),
    value = c(1.0, 2.5, 2.0)
  )
  
  p <- mw_plot_trend(df)
  
  expect_s3_class(p, "ggplot")
})

test_that("mw_plot_trend works with color_by", {
  df <- tibble::tibble(
    ref_date = c(2020L, 2021L, 2022L, 2020L, 2021L, 2022L),
    value = c(1, 2, 3, 2, 3, 4),
    geo = c("A", "A", "A", "B", "B", "B")
  )
  
  p <- mw_plot_trend(df, color_by = "geo")
  
  expect_s3_class(p, "ggplot")
})

test_that("mw_plot_trend errors if required columns missing", {
  df_bad <- tibble::tibble(year = c(2020L, 2021L), val = c(1, 2))
  
  expect_error(
    mw_plot_trend(df_bad),
    "ref_date",
    fixed = FALSE
  )
})

test_that("mw_plot_trend errors if color_by column not found", {
  df <- tibble::tibble(
    ref_date = c(2020L, 2021L),
    value = c(1, 2)
  )
  
  expect_error(
    mw_plot_trend(df, color_by = "not_a_col"),
    "color_by column not found",
    fixed = TRUE
  )
})

test_that("mw_plot_compare returns a ggplot object", {
  df <- tibble::tibble(
    ref_date = c(2022L, 2022L, 2022L),
    value = c(1.0, 2.0, 3.0),
    geo = c("A", "B", "C")
  )
  
  p <- mw_plot_compare(df, year = 2022, group_by = "geo")
  
  expect_s3_class(p, "ggplot")
})

test_that("mw_plot_compare errors if year has no rows", {
  df <- tibble::tibble(
    ref_date = c(2022L, 2022L),
    value = c(1.0, 2.0),
    geo = c("A", "B")
  )
  
  expect_error(
    mw_plot_compare(df, year = 1999, group_by = "geo"),
    "no rows found for year",
    fixed = TRUE
  )
})

test_that("mw_plot_compare errors if required columns missing", {
  df_bad <- tibble::tibble(
    ref_date = c(2022L, 2022L),
    value = c(1.0, 2.0)
  )
  
  expect_error(
    mw_plot_compare(df_bad, year = 2022, group_by = "geo"),
    "must contain ref_date, value",
    fixed = TRUE
  )
})

test_that("mw_plot_compare errors if year is not single value", {
  df <- tibble::tibble(
    ref_date = c(2022L, 2022L),
    value = c(1.0, 2.0),
    geo = c("A", "B")
  )
  
  expect_error(
    mw_plot_compare(df, year = c(2021, 2022), group_by = "geo"),
    "year must be a single value",
    fixed = TRUE
  )
})
