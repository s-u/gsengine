parseTablesFromHTML <- function(htmlBlock) {
  htmlStr <- paste(htmlBlock, collapse = "")
  doc <- xml2::read_html(htmlStr, options = "RECOVER")
  
  tblNodes <- xml2::xml_find_all(doc, "//table[contains(@class,'GenTable')]")
  if (length(tblNodes) == 0) {
    tblNodes <- xml2::xml_find_all(doc, "//table")
  }
  if (length(tblNodes) == 0) {
    return(list())
  }
  
  tables <- rvest::html_table(tblNodes, fill = TRUE, header = NA)
  
  keep <- vapply(
    tables,
    function(d) {
      is.data.frame(d) && nrow(d) > 0 && ncol(d) > 0
    },
    logical(1)
  )
  
  if (!any(keep)) {
    return(list())
  }
  
  tblNodes <- tblNodes[keep]
  dfs <- lapply(tables[keep], function(d) {
    if (inherits(d, "tbl_df")) {
      as.data.frame(d)
    } else {
      d
    }
  })
  
  for (i in seq_along(dfs)) {
    node <- tblNodes[[i]]
    heads <- .gs_nearest_gen_headers_xpath(node)
    attr(dfs[[i]], "gen_head_major") <- heads$major
    attr(dfs[[i]], "gen_head_minor") <- heads$minor
    attr(dfs[[i]], "gen_table_id")   <- xml2::xml_attr(node, "id")
  }
  
  return(dfs)
}
