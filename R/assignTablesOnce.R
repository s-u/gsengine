assignTablesOnce <- function(dfs, saveConfig, env) {
  
  if (length(dfs) == 0) {
    return(invisible(NULL))
  }
  
  if (identical(saveConfig$mode, "auto")) {
    target <- saveConfig$target
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
    targets  <- saveConfig$targets
    nNames   <- length(targets)
    nTables  <- length(dfs)
    
    k <- min(nNames, nTables)
    for (i in seq_len(k)) {
      assign(targets[[i]], dfs[[i]], envir = env)  # overwrite explicit names
    }
    
    if (nTables > nNames) {
      prefix <- if (!is.null(saveConfig$default_prefix) && nzchar(saveConfig$default_prefix)) {
        saveConfig$default_prefix
      } else {
        "gs_table_"
      }
      for (j in seq.int(nNames + 1L, nTables)) {
        autoName <- .gs_next_available_name(prefix, env)
        assign(autoName, dfs[[j]], envir = env)
      }
    }
    return(invisible(NULL))
  }
  
  # Fallback: behave like 'auto'
  target <- saveConfig$target
  assign(target, dfs, envir = env)
  return(invisible(NULL))
}
