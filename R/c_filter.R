#' Filter standardized plot-ready data
#'
#' @param df Standardized tibble (output of mw_standardize)
#' @param geo GEO values to keep (character vector)
#' @param indicator Keyword(s) or exact labels (character vector)
#' @param stat Statistic/characteristic filter (e.g., "Percent", "Number of persons")
#' @param start_year Inclusive start year
#' @param end_year Inclusive end year
#' @param exact_indicator If TRUE, exact match; else keyword match
#' @return Filtered tibble
#' @export
mw_filter <- function(df,
                      geo = NULL,
                      indicator = NULL,
                      stat = NULL,
                      start_year = NULL,
                      end_year = NULL,
                      exact_indicator = FALSE) {
  
  x <- tibble::as_tibble(df)
  
  need <- c("table_id", "ref_date", "geo", "indicator", "stat", "value")
  if (!all(need %in% names(x))) {
    rlang::abort("mw_filter(): input is not standardized. Run mw_standardize() first.")
  }
  
  if (!is.null(geo)) {
    x <- dplyr::filter(x, .data$geo %in% .env$geo)
  }
  
  if (!is.null(stat)) {
    x <- dplyr::filter(x, .data$stat %in% .env$stat)
  }
  
  if (!is.null(indicator)) {
    if (exact_indicator) {
      x <- dplyr::filter(x, .data$indicator %in% .env$indicator)
    } else {
      pat <- paste(indicator, collapse = "|")
      x <- dplyr::filter(x, grepl(pat, .data$indicator, ignore.case = TRUE))
    }
  }
  
  if (!is.null(start_year)) x <- dplyr::filter(x, .data$ref_date >= as.integer(start_year))
  if (!is.null(end_year))   x <- dplyr::filter(x, .data$ref_date <= as.integer(end_year))
  
  x
}
