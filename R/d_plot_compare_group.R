#' Snapshot plot: compare groups (e.g., Gender, Age group) within one GEO
#'
#' @param df A data frame (raw StatsCan or standardized)
#' @param year Year to filter (default 2024)
#' @param geo Which GEO to focus on (default "Canada (excluding territories)")
#' @param group_col Which column to compare (default "Gender")
#' @param topic_col Column name for topic (default "topic"; set NULL if not available)
#' @param topic_keep Optional topics to keep (e.g., your top 3)
#' @param value_col Value column (default "VALUE")
#' @param geo_col GEO column (default "GEO")
#' @param year_col Year column (default "REF_DATE")
#' @param characteristic_keep Keep only certain Characteristics (default avoids CI rows)
#' @return ggplot object
#' @export
mw_plot_snapshot_group <- function(df,
                                   year = 2024,
                                   geo = "Canada (excluding territories)",
                                   group_col = "Gender",
                                   topic_col = "topic",
                                   topic_keep = NULL,
                                   value_col = "VALUE",
                                   geo_col = "GEO",
                                   year_col = "REF_DATE",
                                   characteristic_keep = c("Percent", "Rate", "Number of persons")) {
  stopifnot(is.data.frame(df))
  
  needed <- c(value_col, geo_col, year_col, group_col)
  miss <- setdiff(needed, names(df))
  if (length(miss) > 0) {
    stop("Missing columns: ", paste(miss, collapse = ", "))
  }
  
  suppressPackageStartupMessages({
    library(dplyr)
    library(ggplot2)
  })
  
  df2 <- df %>%
    filter(!is.na(.data[[value_col]])) %>%
    filter(.data[[year_col]] == year) %>%
    filter(.data[[geo_col]] == geo)
  
  if ("Characteristics" %in% names(df2)) {
    df2 <- df2 %>% filter(.data[["Characteristics"]] %in% characteristic_keep)
  }
  
  facet_formula <- NULL
  if (!is.null(topic_col) && topic_col %in% names(df2)) {
    if (!is.null(topic_keep)) df2 <- df2 %>% filter(.data[[topic_col]] %in% topic_keep)
    facet_formula <- as.formula(paste("~", topic_col))
  }
  
  group_vars <- c(group_col)
  if (!is.null(topic_col) && topic_col %in% names(df2)) group_vars <- c(group_vars, topic_col)
  
  df_plot <- df2 %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(val = mean(.data[[value_col]], na.rm = TRUE), .groups = "drop")
  
  p <- ggplot(df_plot, aes(x = reorder(.data[[group_col]], val), y = val)) +
    geom_col() +
    coord_flip() +
    labs(
      title = paste0("Snapshot (", year, "): ", geo),
      x = NULL,
      y = NULL
    ) +
    theme_minimal()
  
  if (!is.null(facet_formula)) p <- p + facet_wrap(facet_formula, scales = "free_y")
  
  p
}
