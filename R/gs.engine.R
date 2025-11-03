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
      
      response <- character()
      if (isTRUE(options$echo)) {
        response <- c(response, "```gs", options$code, "```")
      }
      ## Handle case where evaluation is disabled
      if (isTRUE(options$eval)) {
        ## send the command
        msg <- gsio.msg(io, paste(options$code, collapse = "\n"))
        response <- c(response, tryCatch(processOutput(msg, io),
          error = function(e) c("**ERROR while processing output**:", "```", as.character(e), "```")
        ))
      }
      return(paste(response, collapse = "\n"))
    }
  })
}

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
    cleaned_html <- paste0(vapply(xml_children(body_node), as.character, ""), collapse = "")
  } else {
    # Fallback: use full document minus doctype
    cleaned_html <- as.character(doc)
    cleaned_html <- sub("(?is)^<!DOCTYPE[^>]*>", "", cleaned_html, perl = TRUE)
  }

  # Trim leading/trailing whitespace
  cleaned_html <- trimws(cleaned_html)

  cleaned_html
}

## implement this function to handle the GS output
processOutput <- function(msg, io) {
  ## custom handler for experiments
  if (is.function(.GlobalEnv$processGenstatOutput)) {
    processGenstatOutput(msg, io)
  } else {
    ## process one entry at a time
    as.character(unlist(lapply(msg, function(o) {
      if (o$cmd == "OUT" && o$type == "HTML") {
        if (is.function(.GlobalEnv$processGenstatHtmlOutput)) {
          .GlobalEnv$processGenstatHtmlOutput(o$content, io)
        } else if (!isEmpty(o$content)) {
          stripGenVerbatim(o$content)
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