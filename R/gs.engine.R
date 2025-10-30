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
gs.engine <- function (host = Sys.getenv("GENSTAT_HOST", "localhost"),
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
            gsio.msg(io, NULL, wait=FALSE)
        }
    
        ## Function to process Genstat code execution
        function(options) {
            response <- character()
            if (isTRUE(options$echo))
                response <- c(response, "```gs", options$code, "```")
            ## Handle case where evaluation is disabled
            if (isTRUE(options$eval)) {
            ## send the command
            msg <- gsio.msg(io, paste(options$code, collapse="\n"))
            response <- c(response, tryCatch(processOutput(msg, io),
                                             error=function(e) c("**ERROR while processing output**:","```",as.character(e),"```")))
            }
            return(paste(response, collapse="\n"))
        }
    })
}

## This function tests to see if the vector consists of nothing but PRE and SPAN
isEmpty <- function(output){
  # combine into a single string for parsing
  html_str <- paste(output, collapse = "")
  doc <- read_html(html_str)
  
  # extract all text nodes
  text_nodes <- xml_text(xml_find_all(doc, ".//text()"))
  
  if(!identical(text_nodes, character(0))) {
    # clean and test
    text_nodes = trimws(text_nodes)
    
    length(text_nodes[text_nodes != ""]) == 0
  } else {
    TRUE
  }
}

## implement this function to handle the GS output
processOutput <- function(msg, io) {
    ## custom handler for experiments
    if (is.function(.GlobalEnv$processGenstatOutput))
        processGenstatOutput(msg, io)
    else {
        ## process one entry at a time
        as.character(unlist(lapply(msg, function(o) {
            if (o$type == "HTML") {
                if (is.function(.GlobalEnv$processGenstatHtmlOutput))
                    .GlobalEnv$processGenstatHtmlOutput(o$content, io)
                else
                  if(!isEmpty(o$content)){
                    o$content
                  }
            } else if (o$type == "GRAPH") {
                ## save graphs as files
                if (is.null(io$graph.counter))
                    io$graph.counter <- 0L
                i <- io$graph.counter <- io$graph.counter + 1L
                it <- tolower(o$type)
                fn <- paste0("graph_", i, ".", it)
                writeBin(base64decode(o$content), fn)
                paste0("![](", fn, ")\n")
            } else character() ## ignore anyting else
        })))
    }
}
