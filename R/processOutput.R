#' implement this function to handle the GS output
processOutput <- function(msg, io, saveConfig = list(enabled = FALSE)) {
  
  if (is.function(.GlobalEnv$processGenstatOutput)) {
    return(processGenstatOutput(msg, io))
  }
  
  ## clear space for table storage
  if (is.null(io$chunkTables)) {
    io$chunkTables <- list()
  }
  
  as.character(unlist(lapply(msg, function(o) {
    
    if (o$cmd == "OUT" && o$type == "HTML") {
      
      if (is.null(io$table.counter)) {
        io$table.counter <- 0L
      }
      
      html <- character()
      
      if (is.function(.GlobalEnv$processGenstatHtmlOutput)) {
        html <- .GlobalEnv$processGenstatHtmlOutput(o$content, io)
      } else {
        if (!isEmpty(o$content)) {
          html <- stripGenVerbatim(o$content)
          html <- fixTableCols(html, io)
          html <- reduceGenstatBreaks(html, maxBetween = 2)
          html <- trimEmptyLines(html)
        }
      }
      
      if (length(html) > 0 && isTRUE(saveConfig$enabled)) {
        dfs <- parseTablesFromHTML(html)           # <-- parse only
        if (length(dfs) > 0) {
          io$chunkTables <- c(io$chunkTables, dfs) # <-- accumulate
        }
      }
      
      return(if (knitr::is_latex_output()) paste0("```{=latex}\n",
          paste(pandoc::pandoc_convert(text=html, from="html", to="latex"), collapse="\n"), "\n```\n") else html)
    }
    
    if (o$cmd == "GRAPH") {
      
      if (is.null(io$graph.counter)) {
        io$graph.counter <- 0L
      }
      
      i <- io$graph.counter <- io$graph.counter + 1L
      it <- tolower(o$type)
      fn <- paste0("graph_", i, ".", it)
      
      writeBin(base64enc::base64decode(o$content), fn)
      return(paste0("![](", fn, ")\n"))
    }
    
    return(character())
    
  })))
}
