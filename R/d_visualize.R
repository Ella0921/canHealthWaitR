#' Plot trend over time for standardized medwait data
#'
#' @param df Standardized/filtered data (output of mw_standardize() or mw_filter())
#' @param color_by Optional column name to color lines by (e.g., "geo", "indicator")
#' @param title Optional plot title
#'
#' @return A ggplot object
#' @export
mw_plot_trend <- function(df, color_by = NULL, title = "Trend over time") {
  df <- tibble::as_tibble(df)

  need <- c("ref_date", "value")
  if (!all(need %in% names(df))) {
    rlang::abort("mw_plot_trend(): df must contain ref_date and value columns")
  }

  if (!is.null(color_by) && !(color_by %in% names(df))) {
    rlang::abort(paste0("mw_plot_trend(): color_by column not found: ", color_by))
  }

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = .data$ref_date,
      y = .data$value,
      color = if (is.null(color_by)) NULL else .data[[color_by]]
    )
  ) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "Year", y = "Value", title = title) +
    ggplot2::theme_minimal()

  p
}

# ===============================
# Visualization functions
# ===============================

# helper: remove Canada total
mw_filter_province <- function(df) {
  dplyr::filter(df, .data$geo != "Canada (excluding territories)")
}

#' Bar plot: provinces affected by wait time
#'
#' @param std Standardized medwait data (output of mw_standardize() and/or mw_filter()).
#'   Must include at least: ref_date, stat, indicator, geo, value.
#' @param year Integer year to plot (default 2024).
#'
#' @return A ggplot object
#' @export
mw_plot_affected_province <- function(std, year = 2024) {

  d <- std |>
    dplyr::filter(
      .data$ref_date == year,
      .data$stat == "Number of persons",
      stringr::str_detect(.data$indicator, "affected")
    ) |>
    mw_filter_province()

  ggplot2::ggplot(
    d,
    ggplot2::aes(x = .data$value, y = forcats::fct_reorder(.data$geo, .data$value))
  ) +
    ggplot2::geom_col(fill = "steelblue") +
    ggplot2::labs(
      title = "People affected by healthcare wait times (2024)",
      x = "Number of persons",
      y = NULL
    ) +
    ggplot2::theme_minimal()
}

#' Stacked (100%) bar: satisfaction vs dissatisfaction by province
#' Uses "Number of persons" to compute within-province percentages
#'
#' @param std Standardized medwait data (output of mw_standardize() and/or mw_filter()).
#'   Must include at least: ref_date, stat, indicator, geo, value.
#' @param year Integer year to plot (default 2024).
#'
#' @return A ggplot object
#' @export
mw_plot_satisfaction_stack <- function(std, year = 2024) {

  d <- std |>
    dplyr::filter(
      .data$ref_date == year,
      .data$stat == "Number of persons",
      .data$indicator %in% c(
        "Satisfaction with wait time - Very satisfied or satisfied",
        "Satisfaction with wait time - Dissatisfied or very dissatisfied"
      ),
      .data$geo != "Canada (excluding territories)",
      !is.na(.data$value)
    ) |>
    dplyr::group_by(.data$geo) |>
    dplyr::mutate(pct = .data$value / sum(.data$value)) |>
    dplyr::ungroup()

  if (nrow(d) == 0) {
    stop("No rows after filtering. Check indicator names and stat labels with table(std$stat) and unique(std$indicator).")
  }

  ggplot2::ggplot(
    d,
    ggplot2::aes(
      x = .data$pct,
      y = forcats::fct_reorder(.data$geo, .data$pct, .fun = sum),
      fill = .data$indicator
    )
  ) +
    ggplot2::geom_col(width = 0.8) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::labs(
      title = paste0("Satisfaction with wait times by province (", year, ")"),
      x = "Share of respondents",
      y = NULL,
      fill = NULL
    ) +
    ggplot2::theme_minimal()
}

#' Stacked (100%) bar: wait duration distribution by province
#' Uses "Number of persons" to compute within-province percentages
#'
#' @param std Standardized medwait data (output of mw_standardize() and/or mw_filter()).
#'   Must include at least: ref_date, stat, indicator, geo, value.
#' @param year Integer year to plot (default 2024).
#'
#' @return A ggplot object
#' @export
mw_plot_wait_duration_stack <- function(std, year = 2024) {

  d <- std |>
    dplyr::filter(
      .data$ref_date == year,
      .data$stat == "Number of persons",
      .data$indicator %in% c(
        "Wait time, less than 3 months",
        "Wait time, 3 months to less than 6 months",
        "Wait time, 6 months or more"
      ),
      .data$geo != "Canada (excluding territories)",
      !is.na(.data$value)
    ) |>
    dplyr::group_by(.data$geo) |>
    dplyr::mutate(pct = .data$value / sum(.data$value)) |>
    dplyr::ungroup()

  if (nrow(d) == 0) {
    stop(
      "No rows after filtering. Check stat and indicator labels.\n",
      "Try: table(std$stat) and unique(std$indicator)"
    )
  }

  # Optional: control the order of stack segments (left-to-right)
  d$indicator <- factor(
    d$indicator,
    levels = c(
      "Wait time, less than 3 months",
      "Wait time, 3 months to less than 6 months",
      "Wait time, 6 months or more"
    )
  )

  ggplot2::ggplot(
    d,
    ggplot2::aes(
      x = .data$pct,
      y = forcats::fct_reorder(.data$geo, .data$pct, .fun = sum),
      fill = .data$indicator
    )
  ) +
    ggplot2::geom_col(width = 0.8) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::labs(
      title = paste0("Distribution of specialist wait times by province (", year, ")"),
      x = "Share of respondents",
      y = NULL,
      fill = "Wait duration"
    ) +
    ggplot2::theme_minimal()
}
