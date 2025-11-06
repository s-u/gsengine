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
#' @param saveConfig List produced by normalize_save_tables_option()
#' @importFrom xml2 read_html xml_find_all xml_find_first xml_text xml_attr
#' @importFrom rvest html_table
#' @importFrom knitr knit_global
extractTablesToEnv <- function(htmlBlock, saveConfig) {
  
  html_str <- paste(htmlBlock, collapse = "")
  doc <- xml2::read_html(html_str, options = "RECOVER")
  
  tbl_nodes <- xml2::xml_find_all(doc, "//table[contains(@class,'GenTable')]")
  if (length(tbl_nodes) == 0) {
    tbl_nodes <- xml2::xml_find_all(doc, "//table")
  }
  
  if (length(tbl_nodes) == 0) {
    return(invisible(NULL))
  }
  
  dfs <- rvest::html_table(tbl_nodes, fill = TRUE, header = NA)
  
  # Normalize to base data.frames; drop empty (0×0)
  dfs <- lapply(
    dfs,
    function(d) {
      if (inherits(d, "tbl_df")) {
        d <- as.data.frame(d)
      }
      if (is.data.frame(d)) {
        if (nrow(d) > 0 && ncol(d) > 0) {
          return(d)
        } else {
          return(NULL)
        }
      } else {
        return(NULL)
      }
    }
  )
  dfs <- Filter(function(x) { !is.null(x) }, dfs)
  
  if (length(dfs) == 0) {
    return(invisible(NULL))
  }
  
  # Attach GenHeadMajor/Minor + id attributes
  for (i in seq_along(dfs)) {
    
    node <- tbl_nodes[[i]]
    
    heads <- .gs_nearest_gen_headers_xpath(node)
    attr(dfs[[i]], "gen_head_major") <- heads$major
    attr(dfs[[i]], "gen_head_minor") <- heads$minor
    attr(dfs[[i]], "gen_table_id")   <- xml2::xml_attr(node, "id")
  }
  
  resolveAssignmentEnv <- function() {
    if (isTRUE(getOption("knitr.in.progress"))) {
      return(knitr::knit_global())
    } else {
      return(globalenv())
    }
  }
  
  env <- resolveAssignmentEnv()
  
  # ---------------- modes ----------------
  
  if (identical(saveConfig$mode, "auto")) {
    
    target <- saveConfig$target
    
    if (exists(target, envir = env, inherits = FALSE)) {
      old <- get(target, envir = env, inherits = FALSE)
      if (is.list(old)) {
        dfs <- c(old, dfs)
      } else {
        dfs <- c(list(old), dfs)
      }
    }
    
    assign(target, dfs, envir = env)
    return(invisible(NULL))
  }
  
  if (identical(saveConfig$mode, "single_or_list")) {
    
    target <- saveConfig$target
    
    if (length(dfs) == 1L) {
      assign(target, dfs[[1]], envir = env)
    } else {
      assign(target, dfs, envir = env)
    }
    
    return(invisible(NULL))
  }
  
  if (identical(saveConfig$mode, "vector_map")) {
    #browser()
    targets <- saveConfig$targets
    n_names <- length(targets)
    n_tables <- length(dfs)
    
    k <- min(n_names, n_tables)
    
    # 1:1 mapping for as many names as there are tables
    for (i in seq_len(k)) {
      
      nm <- targets[[i]]
      val <- dfs[[i]]
      
      if (exists(nm, envir = env, inherits = FALSE)) {
        cur <- get(nm, envir = env, inherits = FALSE)
        if (is.list(cur)) {
          assign(nm, c(cur, list(val)), envir = env)
        } else {
          assign(nm, list(cur, val), envir = env)
        }
      } else {
        assign(nm, val, envir = env)
      }
    }
    
    # More tables than names → assign numbered defaults
    if (n_tables > n_names) {
      
      prefix <- if (!is.null(saveConfig$default_prefix) && nzchar(saveConfig$default_prefix)) {
        saveConfig$default_prefix
      } else {
        "gs_table_"
      }
      
      for (j in seq.int(n_names + 1L, n_tables)) {
        
        auto_name <- .gs_next_available_name(prefix, env)
        val <- dfs[[j]]
        
        assign(auto_name, val, envir = env)
      }
    }
    
    return(invisible(NULL))
  }
  
  # Fallback: behave like 'auto'
  target <- saveConfig$target
  if (exists(target, envir = env, inherits = FALSE)) {
    old <- get(target, envir = env, inherits = FALSE)
    if (is.list(old)) {
      dfs <- c(old, dfs)
    } else {
      dfs <- c(list(old), dfs)
    }
  }
  assign(target, dfs, envir = env)
  return(invisible(NULL))
}

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
