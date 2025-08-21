## NOTE: roxygen2 is brain-dead, becuase it does *not* concatenate
## descriptions from individual functions (it just drops them), so
## the first function *has* to include descriptions of all functions,
## as stupid as it sounds.

#' @title Genstat Messenger TCP/IO Communication Protocol Functions
#'
#' @description
#' \code{gsio.connect} connects to the Messenger server according
#' to the \code{host} and \code{port} arguments. Typically you want
#' to call \code{gsio.greeting()} on the resulting connection to
#' start the communication.
#'
#' \code{gsio.msg} sends a message to the Messenger and waits for a response.
#' Any unmatched, named arguments in \dots will be ignored.
#'
#' \code{gsio.greeting} receives a greeting response from the Messenger,
#' i.e., the first response after connecting.
#' 
#' The \code{close} method closes the connection to the GenStat Messenger.
#'
#' The following functions are low-level and typically not used directly
#' (use \code{gsio.msg} instead): \code{gsio.send} sends content to the
#' Messenger, \code{gsio.recv} receives a reply from the Messenger.
#' 
#' @details A typical communication with the Messenger starts with
#'   \code{gsio.connect()}, followed by \code{gsio.greeting()}, one or more
#'   calls to \code{gsio.msg()} and final \code{close()}.
#'   In most cases the messages to sent to the Messenger are strings
#'   representing code to evaluate in GenStat.
#'
#'   Since Messenger 1.1 there are special commands that will be interpreted
#'   by the Messenger instead of passing them to GenStat. Those messages start
#'   with \code{"#:"}. Should you need to send a string to GenStat that also
#'   starts with \code{"#:"} (uncommon) then precede the string with \code{"#:"}
#'   to tell the Messenger that it should be passed through, so \code{"#:#:FOO"}
#'   will result in \code{"#:FOO"} being sent to GenStat.
#'
#'   Which special commands are supported depends on the Messenger version.
#'   As of 1.1 \code{"#:SET_OUTPUT_TYPE:HTML"} will set the output type
#'   (valid values are \code{TEXT}, \code{RTF} and \code{HTML}) and restart
#'   GenStat with the new setting. \code{"#:RESTART"} will restart the
#'   GetStat server without changing the current output settings. NOTE: In both
#'   cases the GetStat server currently sends \emph{two} \code{STAT} responses,
#'   so it is prudent to call \code{gsio.recv(c, wait=FALSE)} immediately after
#'   receiving the message response to clear the extra \code{STAT} response.
#' 
#' @param host string, name (or IP address) or the host to connect to
#' @param port integer, TCP port to connect to
#' @param timeout real, time (in seconds, can be fractional) for communication
#'   attempts. 
#'
#' @return \code{gsio.connect}: an object of the class \code{"gsio"} which
#'         can be used for subsequent communication.
#'
#' @name gsio
#' @export
gsio.connect <- function(host="localhost", port=8085L, timeout=1) {
    e <- new.env(parent=emptyenv())
    e$conn <- socketConnection(host = host, port = port, blocking = TRUE, open = "r+", timeout=timeout)
    e$host <- host
    e$port <- port
    e$valid <- TRUE
    class(e) <- "gsio"
    e
}

#' @export
print.gsio <- function(x, ...) {
    cat("GenStat-IO connection to ", x$host,":", x$port,"\n", sep='')
}

#' @param io \code{"gsio"} object as obtained from \code{gsio.connect()}
#' @param \dots content to send, will be pasted together as string without
#'        a separator.
#' @return \code{gsio.send}: undefined (currently \code{NULL})
#' @rdname gsio
#' @seealso \code{link{gsio.msg()}}
#' @export
gsio.send <- function(io, ...) {
    writeLines(paste(..., sep=''), io$conn)
}

gsio.err <- function(e, ...) {
    if (e$valid) {
        e$valid <- FALSE
        close(e$conn)
        e$conn <- NULL
    }
    e$valid <- FALSE
    e$err <- paste(..., sep='', collapse='\n')
    stop(structure(list(message=e$err, call=NULL, gsio=e), class=c("gsio.error", "error", "condition")))
}

#' @param wait logical scalar, if \code{TRUE} then this function does not return until
#'        a reply is received. Otherwise will return \code{NULL} if no reply was recevied.
#'        Note that if a response header is recevied, the function will still wait until it can read
#'        the entire response in order to prevent inconsistent state of the connection.
#' @return \code{gsio.recv}: Either \code{NULL} (if \code{wait=FALSE} and no reply is present) or a
#'         a reply (see \code{gsio.msg} return description below for the definition of a reply).
#' 
#' @rdname gsio
#' @export
gsio.recv <- function(io, wait=TRUE) {
    l <- character()
    while (!length(l)) {
        l <- readLines(io$conn, 1L)
        if (!length(l) && !wait) return(NULL)
    }

    if (!length(grep("^# ", l)))
        gsio.err(io, "Invalid response stream (expected message frame, got '", l, "')")
    fr <- strsplit(l, " ")[[1]]
    if (length(fr) < 4L)
        gsio.err(io, "Invalid response stream (expected message frame, got '", l, "')")
    len <- as.integer(fr[4])

    p <- character()
    while (length(p) < len) {
        n <- readLines(io$conn, len - length(p))
        if (!length(n))
                gsio.err(io, "Incomplete message frame (need ", len, " lines, got ", length(p), ")")
        p <- c(p, n)
    }
    list(cmd=fr[2], type=fr[3], content=p)
}

#' @param all logical scalar, if \code{TRUE} then all replies until \code{"STAT"} is
#'        received with be collected and returned at the end in a list. Note that if
#'        \code{"STAT"} is the only reply when the list will be empty. If \code{FALSE}
#'        then only the first reply will be returned.
#'       
#' @return \code{gsio.msg}: If \code{all=TRUE} then a list of replies, otherwise a single reply.
#'         A reply is a list with the components \code{"cmd"} (command), \code{"type"} (content type)
#'         and \code{"content"}.
#'         Most common commands are \code{"OUT"} (text output), \code{"GRAPH"} (plot output) and
#'         \code{"STAT"} (end of evaluation, "status" change).
#'         Common types are \code{"TEXT"}, \code{"RTF"} and \code{"HTML"} for text output
#'         types, \code{"PNG"} for graph output and \code{"NULL"} if the reply has no contents.
#'
#' @rdname gsio
#' @export
gsio.msg <- function(io, ..., all=TRUE) {
    l <- list(...)
    if (!is.null(names(l))) l <- l[names(l) == ""]
    if (!length(l)) stop("Nothing to send")
    do.call(gsio.send, c(list(io), l))
    if (!isTRUE(all)) return(gsio.recv(io, wait=TRUE))
    l <- list()
    while(length(res <- gsio.recv(io, wait=TRUE))) {
        if (identical(res$cmd, "STAT"))
            break
        l <- c(l, list(res))
    }
    l
}

#' @param con \code{"gsio"} object as obtained from \code{\link{gsio.connect()}}
#' @rdname gsio
#' @export
close.gsio <- function(con, ...) {
    if (con$valid)
        close(con$conn)
    con$valid <- FALSE
    con$err <- "Connection closed locally."
}

#' @return \code{gsio.greeting}: Greeting reply contents.
#' Messenger verisons 1.1 and higher will reply with a list at least
#' with the components \code{version} and \code{outputType}. The latter can have
#' values \code{"TEXT"}, \code{"HTML"} and \code{"RTF"}. Older versions will only
#' return a string.
#'
#' @importFrom jsonlite fromJSON
#' @rdname gsio
#' @export
gsio.greeting <- function(io) {
    r <- gsio.recv(io, wait=TRUE)
    if (is.null(r$cmd) || !identical(r$cmd, "G5CLI1"))
        gsio.err(io, "Invalid server reply, expected G5CLI1")
    (if (identical(r$type, "JSON")) jsonlite::fromJSON(r$content) else r$content)
}
