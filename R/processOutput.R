#' implement this function to handle the GS output
processOutput <- function(msg, io, saveConfig = list(enabled = FALSE)) {
  
  ## custom handler for experiments
  if (is.function(.GlobalEnv$processGenstatOutput)) {
    processGenstatOutput(msg, io)
  } else {
    ## process one entry at a time
    as.character(unlist(lapply(msg, function(o) {
      if (o$cmd == "OUT" && o$type == "HTML") {
        if (is.null(io$table.counter)) {
          io$table.counter <- 0L
        }
        
        html <- character()
        
        if (is.function(.GlobalEnv$processGenstatHtmlOutput)) {
          html <- .GlobalEnv$processGenstatHtmlOutput(o$content, io)
        } else if (!isEmpty(o$content)) {
          html <- stripGenVerbatim(o$content)
          html <- fixTableCols(html, io)
          
          if(length(html) > 0 && isTRUE(saveConfig)){
            extractTablestoEnv(html, saveConfig)
          }
          
          ## return the HTML to knitr
          html
        }
      } else if (o$cmd == "GRAPH") {
        ## save graphs as files
        if (is.null(io$graph.counter)) {
          io$graph.counter <- 0L
        }
        i <- io$graph.counter <- io$graph.counter + 1L
        it <- tolower(o$type)
        fn <- paste0("graph_", i, ".", it)
        writeBin(base64decode(o$content), fn)
        paste0("![](", fn, ")\n")
      } else {
        character()
      } ## ignore anyting else
    })))
  }
}