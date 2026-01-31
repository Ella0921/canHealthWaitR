#' List curated tables
#' @return tibble with table_key, product_id, title
#' @export
mw_list_tables <- function() {
  mw_registry()
}

#' Resolve a table identifier into a registry row
#' @param x Either table_key (character) or product_id (numeric/integer)
#' @return one-row tibble (table_key, product_id, title)
#' @export
mw_resolve_table <- function(x) {
  reg <- mw_registry()
  
  if (is.character(x) && length(x) == 1) {
    out <- reg[reg$table_key == x, , drop = FALSE]
  } else {
    pid <- as.integer(x)
    out <- reg[reg$product_id == pid, , drop = FALSE]
  }
  
  if (nrow(out) == 0) {
    rlang::abort("Unknown table identifier. Use mw_list_tables() to see valid options.")
  }
  
  out
}

#' Search within curated tables by keyword (optional)
#' @param query keyword string
#' @return matching rows from mw_registry()
#' @export
mw_search_tables <- function(query) {
  stopifnot(is.character(query), length(query) == 1)
  reg <- mw_registry()
  reg[grepl(tolower(query), tolower(reg$title)), , drop = FALSE]
}