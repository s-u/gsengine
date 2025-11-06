trimEmptyLines = function(html){
  
  if (!length(html)) {
    return(html)
  }
  
  ## Remove empty lines
  ## After you build `html`, before returning it:
  cleanedHTML <- gsub("(<table[^>]*>)(\\s*\\n)+", "\\1\n", html, perl = TRUE)
  
  ## Remove blank lines just before <tbody>
  cleanedHTML <- gsub("(\\s*\\n)+(<tbody>)", "\\2", cleanedHTML, perl = TRUE)
  
  ## Remove blank lines after <style> blocks or before <table>
  cleanedHTML <- gsub("(<style[^>]*>\\s*</style>)(\\s*\\n)+", "\\1\n", cleanedHTML, perl = TRUE)
  
  cleanedHTML
}