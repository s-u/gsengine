## This function tests to see if the vector consists of nothing but PRE and SPAN
#' @import xml2
isEmpty <- function(output) {
  # combine into a single string for parsing
  html_str <- paste(output, collapse = "")
  doc <- read_html(html_str)
  if (!inherits(doc, "xml_node")) {
    return(TRUE)
  }
  
  # extract all text nodes
  text_nodes <- xml_text(xml_find_all(doc, ".//text()"))
  
  if (!identical(text_nodes, character(0))) {
    # clean and test
    text_nodes <- trimws(text_nodes)
    
    length(text_nodes[text_nodes != ""]) == 0
  } else {
    TRUE
  }
}
