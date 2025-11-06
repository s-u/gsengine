#' Genstat Socket Engine for knitr
#'
#' This function creates a custom knitr engine for executing Genstat code by connecting
#' to a Genstat server over a socket. The engine sends Genstat code, receives output,
#' and renders it in R Markdown documents with support for formatted tables, warnings,
#' and embedded plots.
#'
#' @param host A character string specifying the host name or IP address of the Genstat server.
#'             Defaults to `"localhost"` or the environment variable `GENSTAT_HOST`.
#' @param port An integer specifying the port number to connect to on the Genstat server.
#'             Defaults to `8085` or the environment variable `GENSTAT_PORT`.
#' @param timeout numeric, number of seconds to wait if no response is coming.
#'
#' @return A function that can be registered as a knitr engine (e.g., via `knitr::knit_engines$set(gs = gs.engine())`)
#'
#' @examples
#' \dontrun{
#' knitr::knit_engines$set(gs = gs.engine())
#' }
#' @importFrom base64enc base64decode
#' @export
#'
gs.engine <- function(host = Sys.getenv("GENSTAT_HOST", "localhost"),
                      port = as.integer(Sys.getenv("GENSTAT_PORT", "8085")),
                      timeout = 1L) {
  local({
    ## Establish a socket connection to the Genstat server
    io <- gsio.connect(host = host, port = port, timeout = timeout)
    ## this is a bit of a hack for now to ease debugging
    .GlobalEnv$.gsio <- io

    ## get greeting
    inf <- gsio.greeting(io)
    if (!is.list(inf)) stop("GS messenger is too old, please upgrade.")
    ## check if output is HTML
    if (!isTRUE(inf$outputType == "HTML")) {
      ## switch to HTML
      ht <- gsio.msg(io, "#:SET_OUTPUT_TYPE:HTML")
      gsio.msg(io, NULL, wait = FALSE)
    }

    ## Function to process Genstat code execution
    function(options) {
      # Add CSS only for HTML-like outputs
      if (knitr::is_html_output(excludes = c("epub"))) {
        # Add once per document (cheap to call repeatedly; knitr dedupes by identity)
        knitr::knit_meta_add(list(html_dependency_gsengine()))
      }
      
      ## Get the arguments for saving tables (if any) to data.frames
      saveConfig <- getSaveTablesOptions(options$saveTables, options$label)
      includeFlag <- if (is.null(options$include)) {
        TRUE 
      } else {
        isTRUE(options$include)
      }
      
      # reset per-chunk buffer
      .GlobalEnv$.gsio$chunkTables <- list()
      
      piece <- character()
      
      if (isTRUE(options$eval)) {
        msg <- gsio.msg(io, paste(options$code, collapse = "\n"))
        piece <- tryCatch(
          processOutput(msg, io, saveConfig = saveConfig),  # no change needed here
          error = function(e) c("**ERROR while processing output**:", "```", as.character(e), "```")
        )
        
        # ---- assign once per chunk ----
        if (isTRUE(saveConfig$enabled)) {
          env <- resolveAssignmentEnv()
          dfs <- .GlobalEnv$.gsio$chunkTables
          assignTablesOnce(dfs, saveConfig, env)
          .GlobalEnv$.gsio$chunkTables <- list()
        }
      }
      
      if (!isTRUE(includeFlag)) {
        # include=FALSE: run code, but return nothing (suppress code + output)
        return("")
      }
      
      response <- character()
      if (isTRUE(options$echo)) {
        response <- c(response, "```gs", options$code, "```")
      }
      
      if (!identical(options$results, "hide")) {
        response <- c(response, piece)
      }

      paste(response, collapse = "\n")
    }
  })
}

