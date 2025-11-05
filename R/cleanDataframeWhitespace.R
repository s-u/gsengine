cleanDataframeWhitespace <- function(df) {
  # Trim cell text
  df[] <- lapply(df, function(x) {
    if (is.character(x)) trimws(x) else x
  })
  
  # Drop all-empty columns
  keep_col <- vapply(df, function(x) any(!(is.na(x) | x == "")), TRUE)
  df <- df[ , keep_col, drop = FALSE]
  
  # Drop all-empty rows
  keep_row <- apply(df, 1L, function(r) any(!(is.na(r) | r == "")))
  df <- df[keep_row, , drop = FALSE]
  
  # Attempt numeric conversion where sensible
  df[] <- lapply(df, function(x) {
    if (!is.character(x)) return(x)
    suppressWarnings({
      xn <- as.numeric(gsub(",", "", x))
      if (sum(!is.na(xn)) >= floor(0.7 * length(x))) xn else x
    })
  })
  df
}
