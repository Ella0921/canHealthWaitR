#' Standardize healthcare wait time data
#'
#' @param df Raw data frame retrieved from the API
#' @return A standardized tibble
#' @export
mw_standardize <- function(df) {
  # Automatically clean and normalize column names
  df_clean <- janitor::clean_names(df)
  
  df_standard <- df_clean |>
    dplyr::rename(
      province = dplyr::any_of(c("geo", "geography")),
      year = dplyr::any_of(c("ref_date", "reference_period")),
      indicator = dplyr::any_of(c("dg_guid", "indicator_name")),
      val = dplyr::any_of(c("value", "coordinate"))
    ) |>
    dplyr::mutate(
      year = as.integer(substr(as.character(.data$year), 1, 4)),
      val = as.numeric(.data$val)
    )
  
  return(tibble::as_tibble(df_standard))
}

