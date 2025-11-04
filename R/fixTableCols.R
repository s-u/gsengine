#' Inject per-table nth-of-type CSS from <col> alignment/widths, then remove <col>s
#' @param output Character vector (HTML fragments) to preprocess
#' @param io Environment which contains, among other things, a table counter which is necessary to give the tables unique ids.
#' @return A single character string of cleaned HTML
#' @importFrom xml2 read_html xml_find_all xml_find_first xml_children xml_attr xml_set_attr xml_add_sibling xml_remove xml_add_child xml_new_root
fixTableCols <- function(output, io) {
  html_str <- paste(output, collapse = "")
  doc <- xml2::read_html(html_str, options = "RECOVER")
  
  if (!inherits(doc, "xml_node")) {
    return(character())
  }
  
  # helper: trim + lowercase safely
  tolower_trim <- function(x) {
    if (is.na(x) || is.null(x)) { 
      return(NA_character_) 
    }
    
    x <- trimws(x)
    
    if (nzchar(x)) { 
      tolower(x) 
    } else { 
      NA_character_ 
    }
  }
  
  tables <- xml2::xml_find_all(doc, "//table[.//col]")
  
  if (length(tables) > 0) {
    for (i in seq_along(tables)) {
      tbl <- tables[[i]]
      
      # ensure table has an id we can scope to
      tbl_id <- xml2::xml_attr(tbl, "id")
      
      if (is.na(tbl_id) || !nzchar(tbl_id)) {
        io$table.counter = io$table.counter + 1
        tbl_id <- sprintf("gs-table-%d", io$table.counter)
        xml2::xml_set_attr(tbl, "id", tbl_id)
      }
      
      # collect all <col> under this table (across any colgroup)
      cols <- xml2::xml_find_all(tbl, ".//col")
      n <- length(cols)
      
      if (n == 0) {
        next
      }
      
      # Build CSS rules
      rules <- character(0)
      
      for (k in seq_len(n)) {
        colk <- cols[[k]]
        align <- tolower_trim(xml2::xml_attr(colk, "align"))
        width <- trimws(xml2::xml_attr(colk, "width"))
        
        decls <- character(0)
      
        if (!is.na(align) && align %in% c("left", "right", "center", "justify")) {
          decls <- c(decls, sprintf("text-align: %s !important;", align))
        }
        
        if (!is.na(width) && nzchar(width)) {
          # keep whatever unit was supplied (%, px, etc.)
          decls <- c(decls, sprintf("width: %s !important;", width))
        }
        
        if (length(decls) > 0) {
          # Scope to this table only
          sel <- sprintf("#%s td:nth-of-type(%d), #%s th:nth-of-type(%d)", tbl_id, k, tbl_id, k)
          rules <- c(rules, sprintf("%s { %s }", sel, paste(decls, collapse = " ")))
        }
      }
      
      if (length(rules) > 0) {
        css <- paste(rules, collapse = "\n")
        style_node <- xml2::read_html(
          sprintf("<style>\n%s\n</style>", css),
          options = "RECOVER"
        )
        
        # insert the <style> immediately BEFORE the table
        xml2::xml_add_sibling(tbl, xml2::xml_find_first(style_node, "//style"), .where = "before")
      }
      
      # remove all <col> and any empty <colgroup>
      xml2::xml_remove(cols)
      empty_groups <- xml2::xml_find_all(tbl, ".//colgroup[not(col)]")
      if (length(empty_groups) > 0) {
        xml2::xml_remove(empty_groups)
      }
    }
  }
  
  # Return only the BODY children if present
  body <- xml2::xml_find_first(doc, "//body")
  
  if (!is.na(body)) {
    out <- paste0(vapply(xml2::xml_children(body), as.character, ""), collapse = "")
  } else {
    out <- as.character(doc)
    out <- sub("(?is)^<!DOCTYPE[^>]*>", "", out, perl = TRUE)
  }
  trimws(out)
}
