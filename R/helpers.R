# Helper: nearest GenHeadMajor/Minor using XPath only (no sibling walking)
.gs_nearest_gen_headers_xpath <- function(tbl_node) {
  maj_node <- xml2::xml_find_first(tbl_node, "preceding-sibling::span[contains(@class,'GenHeadMajor')]")
  min_node <- xml2::xml_find_first(tbl_node, "preceding-sibling::span[contains(@class,'GenHeadMinor')]")
  
  major <- if (!is.na(maj_node)) trimws(xml2::xml_text(maj_node)) else NA
  minor <- if (!is.na(min_node)) trimws(xml2::xml_text(min_node)) else NA
  
  return(list(major = major, minor = minor))
}

.gs_next_available_name <- function(prefix, env) {
  i <- 1L
  candidate <- paste0(prefix, i)
  while (exists(candidate, envir = env, inherits = FALSE)) {
    i <- i + 1L
    candidate <- paste0(prefix, i)
  }
  return(candidate)
}
