#' Curated registry of our project tables
#' @keywords internal
mw_registry <- function() {
  tibble::tibble(
    table_key  = c("wait_time", "health_indicators", "fn_access"),
    product_id = c(13100961L, 13100962L, 41100081L),
    title = c(
      "Wait time for an initial consultation with a medical specialist in the past 12 months",
      "Health indicators, Survey on Health Care Access and Experiences - Primary and Specialist Care",
      "Selected characteristics of health care access and experiences by First Nations people living off reserve"
    )
  )
}

#' Default cache directory
#' @keywords internal
mw_cache_dir <- function() {
  file.path(tools::R_user_dir("medwait", which = "cache"), "statcan_wds")
}