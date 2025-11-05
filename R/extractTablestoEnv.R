#' Parse HTML tables and assign as data frames/tibbles into the knit env
#'
#' Supports three modes:
#'  - mode="auto": assign list to a name like gs_tables_<chunklabel>
#'  - mode="single_or_list": assign a single table (if n=1) or a list to a given name
#'  - mode="vector_map": map first k tables to k names
#'
#' Requires: xml2, rvest
#'
#' @importFrom xml2 read_html xml_find_all
#' @importFrom rvest html_table
#' @importFrom knitr knit_global
#'
#' @param htmlBlock Character vector of HTML (from Genstat output)
#' @param save_cfg List produced by normalize_save_tables_option()
#' @return Invisibly returns NULL after assigning parsed tables into knit_global()
extractTablesToEnv <- function(htmlBlock, saveConfig) {
  html_str <- paste(htmlBlock, collapse = "")
  doc <- xml2::read_html(html_str, options = "RECOVER")
  
  # Prefer Genstat tables if present; otherwise fall back to all tables
  tbl_nodes <- xml2::xml_find_all(doc, "//table[contains(@class,'GenTable')]")
  if (!length(tbl_nodes)) {
    tbl_nodes <- xml2::xml_find_all(doc, "//table")
  }
  
  if (!length(tbl_nodes)) {
    return(invisible(NULL))
  }
  
  # Convert to data frames (rvest::html_table returns list of data.frames)
  dfs <- rvest::html_table(tbl_nodes, fill = TRUE, header = NA)
  
  # Optional: light cleanup – trim whitespace only rows/cols
  dfs <- lapply(dfs, cleanDataframeWhitespace)
  
  env <- knitr::knit_global()
  
  if (identical(saveConfig$mode, "auto")) {
    assign(saveConfig$target, dfs, envir = env)
    
  } else if (identical(saveConfig$mode, "single_or_list")) {
    if (length(dfs) == 1L) {
      assign(saveConfig$target, dfs[[1]], envir = env)
    } else {
      assign(saveConfig$target, dfs, envir = env)
    }
    
  } else if (identical(saveConfig$mode, "vector_map")) {
    n <- min(length(saveConfig$targets), length(dfs))
    for (i in seq_len(n)) {
      assign(saveConfig$targets[[i]], dfs[[i]], envir = env)
    }
  }
  
  invisible(NULL)
}
