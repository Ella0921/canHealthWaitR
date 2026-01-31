#' Snapshot plot: compare provinces/regions for selected topics
#'
#' Works best when you filter to one year (e.g., 2024).
#' Expected columns (StatsCan raw): GEO, REF_DATE, VALUE, Characteristics, UOM, and topic column (optional).
#' If you already standardized, adapt the column names below.
#'
#' @param df A data frame (raw StatsCan or standardized)
#' @param year Year to filter (default 2024)
#' @param geo_pattern Keep GEO rows matching this (default excludes territories if your GEO has that wording)
#' @param topic_col Column name for topic (default "topic"; set NULL if you only have one topic in df)
#' @param topic_keep Optional character vector of topics to keep (e.g., your top 3)
#' @param value_col Numeric value column name (default "VALUE")
#' @param geo_col Geography column name (default "GEO")
#' @param year_col Year column name (default "REF_DATE")
#' @param characteristic_keep Keep only these Characteristics (default "Percent" + "Rate" + "Number of persons" style)
#' @return ggplot object
#' @export
mw_plot_snapshot_province <- function(df,
                                      year = 2024,
                                      geo_pattern = NULL,
                                      topic_col = "topic",
                                      topic_keep = NULL,
                                      value_col = "VALUE",
                                      geo_col = "GEO",
                                      year_col = "REF_DATE",
                                      characteristic_keep = c("Percent", "Rate", "Number of persons")) {
  stopifnot(is.data.frame(df))
  
  # Basic column checks
  needed <- c(value_col, geo_col, year_col)
  miss <- setdiff(needed, names(df))
  if (length(miss) > 0) {
    stop("Missing columns: ", paste(miss, collapse = ", "))
  }
  
  suppressPackageStartupMessages({
    library(dplyr)
    library(ggplot2)
    library(rlang)
  })
  
  df2 <- df %>%
    filter(!is.na(.data[[value_col]])) %>%
    filter(.data[[year_col]] == year)
  
  # Optionally filter GEO strings (useful if you want Canada excl. territories etc.)
  if (!is.null(geo_pattern)) {
    df2 <- df2 %>% filter(grepl(geo_pattern, .data[[geo_col]], ignore.case = TRUE))
  }
  
  # Optionally filter Characteristics if present (this prevents CI rows etc.)
  if ("Characteristics" %in% names(df2)) {
    df2 <- df2 %>% filter(.data[["Characteristics"]] %in% characteristic_keep)
  }
  
  # Optional topic logic
  facet_formula <- NULL
  if (!is.null(topic_col) && topic_col %in% names(df2)) {
    if (!is.null(topic_keep)) df2 <- df2 %>% filter(.data[[topic_col]] %in% topic_keep)
    facet_formula <- as.formula(paste("~", topic_col))
  }
  
  # Aggregate in case multiple rows per GEO (e.g., duplicates across other dims)
  group_vars <- c(geo_col)
  if (!is.null(topic_col) && topic_col %in% names(df2)) group_vars <- c(group_vars, topic_col)
  
  df_plot <- df2 %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(val = mean(.data[[value_col]], na.rm = TRUE), .groups = "drop")
  
  p <- ggplot(df_plot, aes(x = reorder(.data[[geo_col]], val), y = val)) +
    geom_col() +
    coord_flip() +
    labs(
      title = paste0("Healthcare snapshot (", year, ")"),
      x = NULL,
      y = NULL
    ) +
    theme_minimal()
  
  if (!is.null(facet_formula)) {
    p <- p + facet_wrap(facet_formula, scales = "free_y")
  }
  
  p
}
