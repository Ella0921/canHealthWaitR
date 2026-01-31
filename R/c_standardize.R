#' Standardize StatCan WDS tables into a plot-ready schema
#'
#' Guarantees columns:
#' - table_id, ref_date, geo, indicator, stat, value, dims
#'
#' @param df Raw table data (e.g., data_13100961)
#' @param table_id StatCan productId as character or numeric (e.g., 13100961)
#' @return Tibble in a standardized plot-ready schema
#' @export
mw_standardize <- function(df, table_id = NULL) {
  x <- tibble::as_tibble(df)
  
  # helper: pick first existing column from candidates
  pick <- function(cands) {
    out <- cands[cands %in% names(x)][1]
    if (is.na(out)) NA_character_ else out
  }
  
  col_ref <- pick(c("REF_DATE", "ref_date"))
  col_geo <- pick(c("GEO", "geo"))
  col_val <- pick(c("VALUE", "value"))
  
  # indicator: most tables use Indicators, but 41100081 uses a long column name
  col_ind <- pick(c(
    "Indicators", "indicators",
    "Selected characteristics of health care access and experiences"
  ))
  
  # stat: 13100961/62 use Characteristics; 41100081 uses Statistics
  col_stat <- pick(c("Characteristics", "Statistics", "characteristics", "statistics"))
  
  missing <- c()
  if (is.na(col_ref))  missing <- c(missing, "REF_DATE")
  if (is.na(col_geo))  missing <- c(missing, "GEO")
  if (is.na(col_ind))  missing <- c(missing, "Indicators OR Selected characteristics ...")
  if (is.na(col_stat)) missing <- c(missing, "Characteristics OR Statistics")
  if (is.na(col_val))  missing <- c(missing, "VALUE")
  
  if (length(missing) > 0) {
    rlang::abort(
      paste0(
        "mw_standardize(): cannot find required columns: ",
        paste(missing, collapse = ", "),
        "\nColumns present: ", paste(names(x), collapse = ", ")
      )
    )
  }
  
  # normalize core columns
  ref_year <- suppressWarnings(as.integer(substr(as.character(x[[col_ref]]), 1, 4)))
  
  core <- tibble::tibble(
    table_id  = if (is.null(table_id)) NA_character_ else as.character(table_id),
    ref_date  = ref_year,
    geo       = as.character(x[[col_geo]]),
    indicator = as.character(x[[col_ind]]),
    stat      = as.character(x[[col_stat]]),
    value     = suppressWarnings(as.numeric(x[[col_val]]))
  )
  
  # pack other columns as dimensions
  other_cols <- setdiff(names(x), c(col_ref, col_geo, col_ind, col_stat, col_val))
  dims <- if (length(other_cols) == 0) {
    vector("list", nrow(x))
  } else {
    purrr::pmap(x[, other_cols, drop = FALSE], function(...) {
      lst <- list(...)
      names(lst) <- other_cols
      lst
    })
  }
  
  dplyr::mutate(core, dims = dims)
}