#' Download a StatCan WDS table and return extracted CSV paths
#' @param product_id StatCan product ID
#' @param language "en" or "fr"
#' @param cache_dir cache directory
#' @param refresh force re-download
#' @return list(data_csv, metadata_csv, zip_path, out_dir)
#' @export
mw_download_table <- function(product_id,
                              language = "en",
                              cache_dir = NULL,
                              refresh = FALSE) {
  
  language <- match.arg(language, c("en", "fr"))
  pid <- as.integer(product_id)
  
  if (is.null(cache_dir)) cache_dir <- mw_cache_dir()
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  
  zip_path <- file.path(cache_dir, sprintf("%d-%s.zip", pid, language))
  out_dir  <- file.path(cache_dir, sprintf("%d-%s", pid, language))
  
  # 1) Get ZIP URL
  endpoint <- sprintf(
    "https://www150.statcan.gc.ca/t1/wds/rest/getFullTableDownloadCSV/%d/%s",
    pid, language
  )
  
  resp <- httr2::request(endpoint) |>
    httr2::req_method("GET") |>
    httr2::req_perform()
  
  payload <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  if (!is.list(payload) || payload$status != "SUCCESS") {
    rlang::abort(paste0("WDS did not return SUCCESS for product_id=", pid))
  }
  zip_url <- payload$object
  
  # 2) Download ZIP
  if (refresh && file.exists(zip_path)) unlink(zip_path)
  if (!file.exists(zip_path)) {
    httr2::request(zip_url) |>
      httr2::req_method("GET") |>
      httr2::req_perform(path = zip_path)
  }
  
  # 3) Unzip
  if (refresh && dir.exists(out_dir)) unlink(out_dir, recursive = TRUE)
  if (!dir.exists(out_dir) || length(list.files(out_dir)) == 0) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    utils::unzip(zip_path, exdir = out_dir)
  }
  
  data_csv <- file.path(out_dir, sprintf("%d.csv", pid))
  meta_csv <- file.path(out_dir, sprintf("%d_MetaData.csv", pid))
  
  if (!file.exists(data_csv)) rlang::abort(paste0("Missing main CSV: ", data_csv))
  if (!file.exists(meta_csv)) rlang::abort(paste0("Missing MetaData CSV: ", meta_csv))
  
  list(
    data_csv = data_csv,
    metadata_csv = meta_csv,
    zip_path = zip_path,
    out_dir = out_dir
  )
}

#' Download all curated tables (3 topics x 2 CSV each)
#' @param language "en" or "fr"
#' @param cache_dir cache directory
#' @param refresh force re-download
#' @return tibble with table_key, product_id, data_csv, metadata_csv
#' @export
mw_download_all_tables <- function(language = "en", cache_dir = NULL, refresh = FALSE) {
  reg <- mw_registry()
  
  paths <- lapply(reg$product_id, function(pid) {
    mw_download_table(pid, language = language, cache_dir = cache_dir, refresh = refresh)
  })
  
  tibble::tibble(
    table_key  = reg$table_key,
    product_id = reg$product_id,
    data_csv = vapply(paths, `[[`, character(1), "data_csv"),
    metadata_csv = vapply(paths, `[[`, character(1), "metadata_csv")
  )
}


#' List downloaded CSV files in cache
#' @param cache_dir cache directory
#' @return tibble of file paths
#' @export
mw_list_downloads <- function(cache_dir = NULL) {
  if (is.null(cache_dir)) cache_dir <- mw_cache_dir()
  if (!dir.exists(cache_dir)) {
    return(tibble::tibble(path = character(), file = character()))
  }
  
  files <- list.files(cache_dir, recursive = TRUE, full.names = TRUE, pattern = "\\.csv$")
  tibble::tibble(
    path = files,
    file = basename(files)
  )
}


