#' Filter healthcare wait time data
#'
#' @param df A standardized data frame
#' @param province Province name(s) to filter
#' @param indicator Keyword used to match indicators
#' @param start_year Starting year for filtering
#' @param end_year Ending year for filtering
#' @return A filtered data frame
#' @export
mw_filter <- function(df, province = NULL, indicator = NULL, start_year = NULL, end_year = NULL) {
  result <- df
  
  if (!is.null(province)) {
    result <- dplyr::filter(result, .data$province %in% !!province)
  }
  
  if (!is.null(indicator)) {
    result <- dplyr::filter(result, grepl(indicator, .data$indicator, ignore.case = TRUE))
  }
  
  if (!is.null(start_year)) {
    result <- dplyr::filter(result, .data$year >= start_year)
  }
  
  if (!is.null(end_year)) {
    result <- dplyr::filter(result, .data$year <= end_year)
  }
  
  return(result)
}
