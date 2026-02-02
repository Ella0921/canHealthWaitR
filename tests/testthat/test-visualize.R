test_that("mw_plot_trend returns a ggplot object", {
  skip_if_not_installed("ggplot2")

  df <- tibble::tibble(
    ref_date = c(2020L, 2021L, 2022L),
    value = c(1.0, 2.5, 2.0)
  )

  p <- mw_plot_trend(df)
  expect_s3_class(p, "ggplot")
})

test_that("mw_plot_trend works with color_by", {
  skip_if_not_installed("ggplot2")

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
    "ref_date and value",
    fixed = TRUE
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

test_that("mw_plot_affected_province returns ggplot with minimal standardized input", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("stringr")
  skip_if_not_installed("forcats")

  std <- tibble::tibble(
    ref_date = c(2024L, 2024L),
    geo = c("Canada (excluding territories)", "British Columbia"),
    stat = c("Number of persons", "Number of persons"),
    indicator = c("People affected by wait times", "People affected by wait times"),
    value = c(100, 50)
  )

  p <- mw_plot_affected_province(std, year = 2024)
  expect_s3_class(p, "ggplot")
})

test_that("mw_plot_affected_province errors if filter results in 0 rows", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("stringr")
  skip_if_not_installed("forcats")

  std <- tibble::tibble(
    ref_date = 2024L,
    geo = "British Columbia",
    stat = "Percent",                 # not "Number of persons" => filtered out
    indicator = "People affected by wait times",
    value = 10
  )

  expect_error(mw_plot_affected_province(std, year = 2024), NA)
})

test_that("mw_plot_satisfaction_stack returns ggplot for valid inputs", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("forcats")
  skip_if_not_installed("scales")

  std <- tibble::tibble(
    ref_date = c(2024L, 2024L, 2024L, 2024L),
    geo = c("British Columbia", "British Columbia", "Alberta", "Alberta"),
    stat = c("Number of persons", "Number of persons", "Number of persons", "Number of persons"),
    indicator = c(
      "Satisfaction with wait time - Very satisfied or satisfied",
      "Satisfaction with wait time - Dissatisfied or very dissatisfied",
      "Satisfaction with wait time - Very satisfied or satisfied",
      "Satisfaction with wait time - Dissatisfied or very dissatisfied"
    ),
    value = c(80, 20, 70, 30)
  )

  p <- mw_plot_satisfaction_stack(std, year = 2024)
  expect_s3_class(p, "ggplot")
})

test_that("mw_plot_satisfaction_stack errors when no rows after filtering", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("forcats")
  skip_if_not_installed("scales")

  std <- tibble::tibble(
    ref_date = 2024L,
    geo = "British Columbia",
    stat = "Number of persons",
    indicator = "Some other indicator",
    value = 1
  )

  expect_error(
    mw_plot_satisfaction_stack(std, year = 2024),
    "No rows after filtering",
    fixed = TRUE
  )
})

test_that("mw_plot_wait_duration_stack returns ggplot for valid inputs", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("forcats")
  skip_if_not_installed("scales")

  std <- tibble::tibble(
    ref_date = c(2024L, 2024L, 2024L, 2024L, 2024L, 2024L),
    geo = c("British Columbia", "British Columbia", "British Columbia",
            "Alberta", "Alberta", "Alberta"),
    stat = rep("Number of persons", 6),
    indicator = c(
      "Wait time, less than 3 months",
      "Wait time, 3 months to less than 6 months",
      "Wait time, 6 months or more",
      "Wait time, less than 3 months",
      "Wait time, 3 months to less than 6 months",
      "Wait time, 6 months or more"
    ),
    value = c(30, 40, 30, 25, 50, 25)
  )

  p <- mw_plot_wait_duration_stack(std, year = 2024)
  expect_s3_class(p, "ggplot")
})

test_that("mw_plot_wait_duration_stack errors when no rows after filtering", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("forcats")
  skip_if_not_installed("scales")

  std <- tibble::tibble(
    ref_date = 2024L,
    geo = "British Columbia",
    stat = "Number of persons",
    indicator = "Not a duration indicator",
    value = 1
  )

  expect_error(
    mw_plot_wait_duration_stack(std, year = 2024),
    "No rows after filtering",
    fixed = TRUE
  )
})
