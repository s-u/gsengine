getSaveTablesOptions <- function(x, chunk_label = NULL) {
  
  # saveTables = FALSE / NULL -> disabled
  if (is.null(x) || identical(x, FALSE)) {
    return(list(enabled = FALSE))
  }
  
  # consider knitr's auto-generated labels as "no label"
  is_auto <- is.null(chunk_label) ||
    !nzchar(chunk_label) ||
    grepl("^unnamed[-.]chunk[-.]", chunk_label)
  
  # saveTables = TRUE -> assign list to a name derived from the chunk label
  if (identical(x, TRUE)) {
    if (is_auto) {
      var <- "gs_tables_last"
    } else {
      var <- paste0("gs_tables_", make.names(chunk_label))
    }
    return(list(enabled = TRUE, mode = "auto", target = var))
  }
  
  # saveTables = "name" -> assign list (or single table) to that symbol
  if (is.character(x) && length(x) == 1L && nzchar(x)) {
    return(list(enabled = TRUE, mode = "single_or_list", target = x))
  }
  
  # saveTables = c("a","b","c") -> map first k tables to those names
  if (is.character(x) && length(x) > 1L) {
    return(list(enabled = TRUE, mode = "vector_map", targets = x))
  }
  
  list(enabled = FALSE)
}
