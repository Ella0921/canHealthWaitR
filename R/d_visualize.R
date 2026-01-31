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


#' Compare values across groups for a single year (bar chart)
#'
#' @param df Standardized/filtered data (output of mw_standardize() or mw_filter())
#' @param year Single year to compare (e.g., 2022)
#' @param group_by Column to compare across (default "geo" or you can use "indicator")
#' @param title Optional plot title
#'
#' @return A ggplot object
#' @export
mw_plot_compare <- function(df, year, group_by = "geo", title = NULL) {
  df <- tibble::as_tibble(df)
  
  need <- c("ref_date", "value", group_by)
  if (!all(need %in% names(df))) {
    rlang::abort(paste0(
      "mw_plot_compare(): df must contain ref_date, value, and '", group_by, "' columns"
    ))
  }
  
  if (length(year) != 1) {
    rlang::abort("mw_plot_compare(): year must be a single value (e.g., 2022)")
  }
  
  year <- as.integer(year)
  
  df_year <- dplyr::filter(df, .data$ref_date == year)
  
  if (nrow(df_year) == 0) {
    rlang::abort(paste0("mw_plot_compare(): no rows found for year ", year))
  }
  
  # If multiple rows per group exist (e.g., multiple indicators), summarize to one value
  df_sum <- df_year |>
    dplyr::group_by(.data[[group_by]]) |>
    dplyr::summarise(value = mean(.data$value, na.rm = TRUE), .groups = "drop")
  
  if (is.null(title)) {
    title <- paste0("Comparison by ", group_by, " in ", year)
  }
  
  ggplot2::ggplot(df_sum, ggplot2::aes(x = .data[[group_by]], y = .data$value)) +
    ggplot2::geom_col() +
    ggplot2::labs(x = group_by, y = "Value", title = title) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}
