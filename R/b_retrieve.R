#' Read a downloaded main CSV into tibble (minimal wrangling)
#' @param product_id StatCan product ID
#' @param language "en" or "fr"
#' @return tibble
#' @export
mw_read_data <- function(product_id, language = "en") {
  paths <- mw_download_table(product_id, language = language)
  readr::read_csv(paths$data_csv, show_col_types = FALSE)
}
