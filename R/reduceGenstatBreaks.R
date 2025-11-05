# Collapse any run of <br> to at most `maxBetween` copies (default 2).
collapseBrRuns <- function(html, maxBetween = 2) {
  if (!length(html)) {
    return(html)
  }
  
  if (maxBetween < 0) {
    maxBetween <- 0
  }
  
  br <- "(?i)<br\\s*/?>"
  if (maxBetween == 0) {
    return(gsub(br, "", html, perl = TRUE))
  }
  
  pattern <- paste0("(?:", br, "\\s*){", maxBetween + 1L, ",}")
  replacement <- paste(rep("<br>", maxBetween), collapse = "")
  return(gsub(pattern, replacement, html, perl = TRUE))
}

# Trim stray <br> directly before block elements like <style> or <table>,
# and directly after them (common in your Genstat output).
trimBrAroundBlocks <- function(html) {
  if (!length(html)) {
    return(html)
  }
  
  # Remove runs of <br> immediately BEFORE <style> or <table>
  html <- gsub("(?i)(?:\\s*<br\\s*/?>)+\\s*(<(?:style|table)\\b)",
               "\\1", html, perl = TRUE)
  
  # Remove runs of <br> immediately AFTER </style> or </table>
  html <- gsub("(?i)(</(?:style|table)>)\\s*(?:<br\\s*/?>\\s*)+",
               "\\1", html, perl = TRUE)
  
  return(html)
}

# Convenience wrapper: reduce Genstat <br> noise in a single call.
reduceGenstatBreaks <- function(html, maxBetween = 2) {
  if (!length(html)) {
    return(html)
  }
  html2 <- collapseBrRuns(html, maxBetween = maxBetween)
  html2 <- trimBrAroundBlocks(html2)
  return(html2)
}
