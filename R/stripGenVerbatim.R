#' Function to strip stuff that looks like this:<SPAN CLASS=GenVerbatim><PRE></PRE></SPAN>
#' from the output.
stripGenVerbatim <- function(output) {
  ## Parse the HTML from a string
  html_str <- paste(output, collapse = "")
  doc <- read_html(html_str, options = "RECOVER")
  
  if (!inherits(doc, "xml_node")) {
    return(character())
  }
  
  # Remove spans that are exactly <span class="GenVerbatim"><pre></pre></span>
  empty_spans <- xml_find_all(
    doc,
    "//span[
       translate(@class,'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz')='genverbatim'
       and count(pre)=1
       and normalize-space(pre)=''
     ]"
  )
  
  if (length(empty_spans) > 0) {
    xml_remove(empty_spans)
  }
  
  # Also remove any standalone empty <pre> blocks
  xml_remove(xml_find_all(doc, '//pre[not(node()) or normalize-space(.)=""]'))
  
  # Get the main content inside <body> if it exists
  body_node <- xml_find_first(doc, "//body")
  if (!is.na(body_node)) {
    # Return only the serialized children of <body>
    cleanedHTML <- paste0(vapply(xml_children(body_node), as.character, ""), collapse = "")
  } else {
    # Fallback: use full document minus doctype
    cleanedHTML <- as.character(doc)
    cleanedHTML <- sub("(?is)^<!DOCTYPE[^>]*>", "", cleanedHTML, perl = TRUE)
  }
  
  # Trim leading/trailing whitespace
  cleanedHTML <- trimws(cleanedHTML)
  
  cleanedHTML
}
